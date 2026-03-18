import requests
import pandas as pd
from bs4 import BeautifulSoup


def scrape_surf_report(url, output_csv_path):
    """
    Scrape surf forecast data from a Surf-Report URL
    and save it as a CSV file.

    Parameters
    ----------
    url : str
        Surf-Report forecast URL
    output_csv_path : str
        Path where the CSV file will be saved
    """

    headers = {
        "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X)"
    }

    response = requests.get(url, headers=headers, timeout=30)
    response.raise_for_status()

    soup = BeautifulSoup(response.text, "lxml")

    days_blocks = soup.find_all("div", class_="forecast-tab")
    data = []

    for day in days_blocks:
        day_name = day.find("div", class_="title").get_text(strip=True)
        content = day.find("div", class_="content")
        lines = content.find_all("div", class_="line")

        for line in lines:
            # ---- Heure ----
            time_div = line.select_one("div.cell.date")
            if not time_div:
                continue
            time = time_div.get_text(strip=True)

            # ---- Vagues ----
            wave_min, wave_max = None, None
            wave_div = line.select_one("div.waves")
            if wave_div:
                values = [
                    s.get_text(strip=True)
                    for s in wave_div.find_all("span")
                    if s.get_text(strip=True).replace(".", "").isdigit()
                ]
                if len(values) >= 2:
                    wave_min, wave_max = values[:2]

            # ---- Vent vitesse ----
            wind_kmh = None
            wind_div = line.select_one("div.wind")
            if wind_div:
                wind_kmh = wind_div.get_text(strip=True)

            # ---- Vent direction ----
            wind_direction = None
            img = line.select_one("div.wind.img img")
            if img and img.has_attr("alt"):
                wind_direction = img["alt"].replace("Orientation vent ", "")

            data.append({
                "day": day_name,
                "time": time,
                "wave_min_m": wave_min,
                "wave_max_m": wave_max,
                "wind_kmh": wind_kmh,
                "wind_direction": wind_direction
            })

    df = pd.DataFrame(data)

    # Nettoyage
    df["wave_min_m"] = pd.to_numeric(df["wave_min_m"], errors="coerce")
    df["wave_max_m"] = pd.to_numeric(df["wave_max_m"], errors="coerce")
    df["wind_kmh"] = pd.to_numeric(df["wind_kmh"], errors="coerce")

    df = df.dropna(
        subset=["wave_min_m", "wave_max_m", "wind_kmh"]
    ).reset_index(drop=True)

    # Sauvegarde
    df.to_csv(output_csv_path, index=False, encoding="utf-8-sig")

    return df
