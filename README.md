# gWidgets2Rgtk4

[![Codecov test coverage](https://codecov.io/gh/iNZightVIT/gWidgets2Rgtk4/branch/main/graph/badge.svg)](https://app.codecov.io/gh/iNZightVIT/gWidgets2Rgtk4?branch=main)

Toolkit backend for [gWidgets2](https://github.com/gWidgets3/gWidgets2) using [Rgtk4](https://github.com/JanMarvin/Rgtk4) (GTK4).

This package replaces [gWidgets2RGtk2](../gWidgets2RGtk2) for new work. The **gWidgets2** public API stays the same; the GTK implementation underneath is new.

```r
options(guiToolkit = "Rgtk4")
library(gWidgets2)
w <- gwindow("Hello")
gbutton("OK", container = w)
```

## Status

Core path and extended widgets are implemented: containers, controls, dialogs, files, chrome (`gmenu` / `gtoolbar` / `gstatusbar`), tables/`gdf`, trees/`gvarbrowser`, calendar, DnD, fonts/CSS. Phase 0 helpers: real `size()`, window position/center API, keystroke handlers, `gtext` margins/`scroll_to`, file-path button icons. `ggraphics` is a unigd + `GtkPicture` spike (no locator). See [docs/widget-inventory.md](docs/widget-inventory.md) for the full checklist and known stubs.

## Documentation

| Doc | |
|-----|--|
| [docs/architecture.md](docs/architecture.md) | Adapter design and class hierarchy |
| [docs/api-mapping.md](docs/api-mapping.md) | RGtk2 → Rgtk4 mapping and GTK4 notes |
| [docs/widget-inventory.md](docs/widget-inventory.md) | Port checklist |

## Related packages

- `gWidgets2` — public API
- `Rgtk4` — GTK4 bindings
- `gWidgets2RGtk2` — legacy GTK2 backend (reference implementation)
