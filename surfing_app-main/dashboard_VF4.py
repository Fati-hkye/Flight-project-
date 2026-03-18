import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from surf_scrap import scrape_surf_report
from good_conditions import calculate_good_conditions  
import dateparser

# ------------------ CONFIG ------------------
st.set_page_config(
    page_title="Surf Conditions Dashboard",
    layout="wide"
)

URL = "https://www.surf-report.com/meteo-surf/carcans-plage-s1013.html"
OUTPUT_PATH = "carcans_surf_data.csv"

# ------------------ STYLE ------------------
st.markdown(
    """
    <style>
    .stApp {
        background-color: #f5f7fa;
    }
    h1, h2, h3 {
        text-align: center;
    }
    </style>
    """,
    unsafe_allow_html=True
)

# ------------------ DATA ------------------
@st.cache_data(ttl=3600)
def get_surf_data():
    df = scrape_surf_report(URL, OUTPUT_PATH)

    df["datetime_str"] = df["day"] + " " + df["time"]
    df["datetime"] = df["datetime_str"].apply(
        lambda x: dateparser.parse(x, languages=["fr"])
    )

    df["mean_wave"] = df[["wave_min_m", "wave_max_m"]].mean(axis=1)
    return df

df = get_surf_data()
df = calculate_good_conditions(df)

# ------------------ METRICS AT TOP (Centered) ------------------
# Calculate best wave and best surf moment for display
best_row = df.loc[(df["wave_max_m"] - 0.05 * df["wind_kmh"]).idxmax()]
best_day = best_row["day"]
best_time = best_row["time"]
best_wave = best_row["wave_max_m"]
best_wind = best_row["wind_kmh"]

max_wave_row = df.loc[df["wave_max_m"].idxmax()]
highest_wave = max_wave_row["wave_max_m"]
highest_wave_day = max_wave_row["day"]
highest_wave_time = max_wave_row["time"]

cols = st.columns([1, 2, 2, 1])  

with cols[1]:
    st.metric(
        label="Highest wave of the week",
        value=f"{highest_wave:.1f} m",
        delta=f"{highest_wave_day} at {highest_wave_time}"
    )
with cols[2]:
    st.metric(
        label="Best moment to surf (coming week)",
        value=f"{best_day}",
        delta=f"Waves: {best_wave:.1f} m | Wind: {best_wind:.0f} km/h"
    )

# ------------------ FILTER ------------------
st.markdown("### When do you want to surf?")
selected_days = st.multiselect(
    "",
    options=df["day"].unique(),
    placeholder="Choose one or more days"
)

df_filtered = df[df["day"].isin(selected_days)] if selected_days else df

# Compute average score only if days selected
if selected_days:
    average_score = df_filtered["good_conditions_pct"].mean()
    average_score = 0 if pd.isna(average_score) else average_score
else:
    average_score = None  

# ------------------ VERDICT ------------------
def surf_verdict(score):
    if score is None:
        return ""
    if score < 40:
        return "Flat & messy "
    elif score < 70:
        return "Rideable "
    return "EPIC SESSION "

verdict = surf_verdict(average_score)

# Choose image path based on verdict 
if verdict.startswith("EPIC"):
    image_path = "epic_session.png"
elif verdict.startswith("Rideable"):
    image_path = "rideable.png"
elif verdict:
    image_path = "flat_messy.png"
else:
    image_path = None  

# ------------------ LAYOUT ------------------

col_left, col_right = st.columns([1, 1])

with col_left:
    if image_path:
        st.image(
            image_path,
            caption=verdict,
            width=450,
        )
    else:
        st.markdown(
            "<p style='text-align:center; font-size:20px; font-weight:600; margin-top:10px;'>Select day(s) to see the surf verdict</p>",
            unsafe_allow_html=True
        )

with col_right:
    # Gauge below the metrics, full width of right column
    gauge_value = average_score if average_score is not None else 0
    gauge_number = {"suffix": "%", "font": {"size": 56}} if average_score is not None else {"suffix": "", "font": {"size": 56}}

    fig = go.Figure(go.Indicator(
        mode="gauge+number" if average_score is not None else "gauge",
        value=gauge_value,
        number=gauge_number,
        gauge={
            "axis": {"range": [0, 100], "tickwidth": 0},
            "bar": {"color": "#ff0dcb", "thickness": 0.35},
            "bgcolor": "rgba(0,0,0,0)",
            "steps": [
                {"range": [0, 40], "color": "#042535"},
                {"range": [40, 70], "color": "#0E5E86"},
                {"range": [70, 100], "color": "#4DB1E2"},
            ],
        }
    ))
    st.plotly_chart(fig, use_container_width=True)

# ------------------ CHARTS ------------------
st.markdown("## Conditions over time")

col1, col2 = st.columns(2, gap="large")

# Always show full dataset for charts
with col1:
    fig_wave = px.line(
        df,
        x="datetime",
        y="mean_wave",
        markers=True,
        labels={"mean_wave": "Wave size (m)", "datetime": "Time"},
    )
    fig_wave.add_hline(y=1, line_dash="dash", line_color="gray")
    fig_wave.update_traces(line=dict(color="#08283f", width=3))
    fig_wave.update_layout(height=420)
    st.plotly_chart(fig_wave, use_container_width=True)

with col2:
    fig_wind = px.line(
        df,
        x="datetime",
        y="wind_kmh",
        markers=True,
        labels={"wind_kmh": "Wind speed (km/h)", "datetime": "Time"},
    )
    fig_wind.add_hline(y=30, line_dash="dash", line_color="gray")
    fig_wind.update_traces(line=dict(color="#158F8F", width=3))
    fig_wind.update_layout(height=420)
    st.plotly_chart(fig_wind, use_container_width=True)

# ------------------ TABLE ------------------
with st.expander("📊 Detailed forecast table"):
    df_table = df_filtered[["day", "time", "mean_wave", "wind_kmh"]].rename(
        columns={
            "day": "Day",
            "time": "Time",
            "mean_wave": "Wave size (m)",
            "wind_kmh": "Wind speed (km/h)",
        }
    )
    st.dataframe(df_table.reset_index(drop=True), use_container_width=True)

# ------------------ EXPLANATION ------------------
with st.expander("ℹ️ How is the surf score calculated?"):
    st.markdown("""
    -  **Wave size** (40%)
    -  **Wind speed** (40%)
    -  **Wind direction** (20%)

    Higher score = cleaner, more surfable conditions.
    """)
