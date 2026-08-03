# iNZight priorities

iNZight is the first real consumer. **Do not block Phase 1 on full iNZight boot** — finish the scaffold/core API first, then use iNZight to order Phase 2.

## Current coupling (as of planning)

Packages under `inzight-library/pkgs/`:

- `iNZight` Depends/Imports: `gWidgets2`, `gWidgets2RGtk2`, and also `RGtk2`, `cairoDevice`
- `iNZightModules` similarly depends on `gWidgets2RGtk2`
- Remotes historically point at patched forks (`tmelliott/gWidgets2`, `iNZightVIT/gWidgets2RGtk2@inz`)

Hard-coded toolkit selection:

```r
# iNZight/R/zzz.R
options("guiToolkit" = "RGtk2")
```

### Direct toolkit leaks (out of scope for this package alone)

Examples that will need an iNZight follow-up PR:

| Area | Issue |
|------|--------|
| `iNZight-package.R` | `@import ... RGtk2 gWidgets2RGtk2 cairoDevice` |
| `misc.R` | `GWidget <- gWidgets2RGtk2:::GWidget`; GTK2 DnD target constants |
| `iNZGUI.R` | `RGtk2::gtkAccelGroup()`, window position via RGtk2 |
| `iNZCodePanel.R` / others | `RGtk2::gtkTextViewSetLeftMargin` etc. |
| `iNZDataViewWidget.R` / `iNZInfoWindow.R` | `RGtk2::gtkImage` |
| Plot toolbar | Historically subclassed toolkit toolbar internals |

Keeping familiar refclass names in gWidgets2Rgtk4 reduces pain but is **not** a promise of `:::` compatibility.

## Likely boot-critical widgets

Heavy gWidgets usage appears across GUI, plot modification windows, change-data windows, menu bar, import, survey design, data view, info windows, etc. Practically, after Phase 1 containers/controls/dialogs, prioritize:

1. **Chrome:** `gmenu`, `gtoolbar`, `gstatusbar`, `gaction`
2. **Plots:** `ggraphics` (unigd + GtkPicture spike; later httpgd/WebView)
3. **Data:** `gtable` / `gdf` (data view widget)
4. **Browse/tree:** `gtree`, `gvarbrowser` as used
5. **Files/dialogs:** largely Phase 1, but verify against import/export flows

## Suggested validation ladder

1. Package smoke demo (window + controls + handler).
2. Minimal “iNZight-shaped” script: main window, paned/notebook, button row, combobox, table stub — no full app.
3. Switch a development iNZight branch to `guiToolkit="Rgtk4"` and `gWidgets2Rgtk4`; fix leaks as they appear.
4. Graphics last-mile: `ggraphics` uses unigd today; consider httpgd + embedded WebView for richer / web results later.

## Tracking

Update [widget-inventory.md](widget-inventory.md) statuses as widgets land. Note iNZight blockers in that file or in issue trackers rather than expanding this package’s public API.
