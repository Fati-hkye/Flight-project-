# Surf Conditions Dashboard 
A Streamlit dashboard that scrapes surf forecast data from Surf-Report (Carcans Plage) and turns it into an interactive “Should I surf?” view with KPIs, a condition gauge, and time-series charts.

## Features
- **Live scraping + caching**: pulls data from Surf-Report and caches it for **1 hour** (`@st.cache_data(ttl=3600)`).
- **Top KPIs**:
  - *Highest wave of the week*
  - *Best moment to surf* (simple score: `wave_max_m - 0.05 * wind_kmh`)
- **Day filter**: select one or more days to compute the **average surf quality score**.
- **Condition gauge (0–100%)**: displayed only once you select at least one day.
- **Verdict image**: shows a local PNG depending on the score:
  - `flat_messy.png`
  - `rideable.png`
  - `epic_session.png`
- **Interactive charts** (Plotly):
  - mean wave size over time
  - wind speed over time
- **Expandable table**: filtered data preview (no index).

## Project structure
surfing_app/
├─ dashboard.py # main Streamlit app (your script)
├─ surf_scrap.py # scraping function: scrape_surf_report()
├─ good_conditions.py # scoring function: calculate_good_conditions()
├─ flat_messy.png
├─ rideable.png
├─ epic_session.png
├─ requirements.txt

## Installation
Create and activate a virtual environment, then install dependencies:

```bash
pip install -r requirements.txt

streamlit run dashboard.py
