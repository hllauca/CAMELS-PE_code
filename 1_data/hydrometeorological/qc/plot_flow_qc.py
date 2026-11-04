"""Inspección visual de las 136 series de caudal observado después del control de calidad.

Cada panel muestra la serie final (flow_OBS_1981_2025.csv, mm/d, escala log) y, encima, los
valores eliminados por cada prueba y los picos sospechosos que se conservan
(qc/flow_OBS_qc_log.csv). Los caudales nulos o negativos se dibujan en el piso del eje (Q_FLOOR).
Salida: qc/plots/flow_qc_partN.png (30 estaciones por página).
"""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib.lines import Line2D
from matplotlib.ticker import NullFormatter

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
OUT = HERE / "plots"
NROW, NCOL = 6, 5
Q_FLOOR = 1e-3  # mm/d
TESTS = {  # prueba → (color, marcador, etiqueta)
    "Flat line (suspect)": ("#F2B701", "o", "Suspect: flat line 7–30 d (kept)"),
    "Flat line": ("#D62728", "o", "Removed: flat line > 30 d"),
    "Linear interpolation": ("#9467BD", "o", "Removed: linear interpolation"),
    "Spike": ("#000000", "x", "Removed: spike (no rain support)"),
    "Gross range": ("#8C564B", "s", "Removed: gross range"),
    "Spike (rain-supported)": ("#FF7F0E", "^", "Suspect: spike with rain support (kept)"),
}

plt.rcParams.update({"font.family": "Arial", "font.size": 6.5, "axes.linewidth": 0.5,
                     "xtick.major.width": 0.5, "ytick.major.width": 0.5})


def main():
    raw = pd.read_csv(ROOT / "1_data/hydrometeorological/flow_OBS_1981_2025.csv")
    dates = pd.to_datetime(raw[["year", "month", "day"]])
    flow = raw.drop(columns=["year", "month", "day"])
    flow.columns = ["PE_" + c for c in flow.columns]
    log = pd.read_csv(HERE / "flow_OBS_qc_log.csv", parse_dates=["date"])
    loc = pd.read_csv(ROOT / "1_data/location/gauge_location.csv")
    OUT.mkdir(exist_ok=True)

    per_page = NROW * NCOL
    pages = int(np.ceil(len(loc) / per_page))
    for p in range(pages):
        sub = loc.iloc[p * per_page:(p + 1) * per_page]
        fig, axs = plt.subplots(NROW, NCOL, figsize=(11.7, 8.3), sharex=True)
        for ax, (_, g) in zip(axs.flat, sub.iterrows()):
            q = flow[g.gauge_id]
            ax.plot(dates, q.clip(lower=Q_FLOOR), color="#1F5FA8", lw=0.35, zorder=2)
            lg = log[log.gauge_id == g.gauge_id]
            for test, (col, mk, _) in TESTS.items():
                t = lg[lg.test == test]
                if len(t):
                    ax.scatter(t.date, t.q_mm.clip(lower=Q_FLOOR), s=3 if mk != "x" else 8, color=col,
                               marker=mk, lw=0.6 if mk == "x" else 0, zorder=3)
            ax.set_yscale("log")
            # Al menos dos décadas en el eje, para que siempre haya marcas 10^n (sin rótulos menores)
            lo, hi = ax.get_ylim()
            if hi / lo < 100:
                mid = np.sqrt(lo * hi)
                ax.set_ylim(mid / 10, mid * 10)
            ax.yaxis.set_minor_formatter(NullFormatter())
            ax.set_xlim(pd.Timestamp("1981-01-01"), pd.Timestamp("2025-12-31"))
            n_rm = int((lg.flag == 4).sum())
            pct = 100 * n_rm / max(n_rm + int(q.notna().sum()), 1)
            ax.set_title(f"{g.gauge_id} – {g.gauge_name}", fontsize=6.5, fontweight="bold", pad=2)
            ax.text(0.01, 0.97, f"{g.gauge_region} · removed {n_rm:,} ({pct:.1f} %)",
                    transform=ax.transAxes, ha="left", va="top", fontsize=5.5, color="#444444", zorder=4,
                    bbox=dict(facecolor="white", edgecolor="none", alpha=0.8, pad=0.8))
            ax.tick_params(labelsize=5.5, length=2)
            ax.grid(True, which="major", color="#E5E5E5", lw=0.3)
        for i, ax in enumerate(axs.flat):
            if i >= len(sub):
                ax.set_visible(False)
            elif i + NCOL >= len(sub):  # último panel de su columna: muestra los años
                ax.tick_params(labelbottom=True)
        for ax in axs[:, 0]:
            ax.set_ylabel(r"Q [mm d$^{-1}$]", fontsize=6)
        handles = [Line2D([], [], color="#1F5FA8", lw=0.8, label="Final series")] + [
            Line2D([], [], color=c, marker=m, ls="none", markersize=3.5, label=l) for c, m, l in TESTS.values()]
        fig.legend(handles=handles, loc="upper center", ncol=7, frameon=False, fontsize=6.2,
                   bbox_to_anchor=(0.5, 0.995))
        fig.text(0.5, 0.004, f"Zero or negative flows are drawn at {Q_FLOOR:g} mm/d (log axis). "
                             f"Page {p + 1} of {pages}.", ha="center", fontsize=5.5, color="#555555")
        fig.tight_layout(rect=(0, 0.012, 1, 0.975), h_pad=0.6, w_pad=0.6)
        f = OUT / f"flow_qc_part{p + 1}.png"
        fig.savefig(f, dpi=300)
        plt.close(fig)
        print("saved", f)


if __name__ == "__main__":
    main()
