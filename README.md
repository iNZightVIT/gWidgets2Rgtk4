# gWidgets2Rgtk4

Toolkit backend for [gWidgets2](https://github.com/gWidgets3/gWidgets2) using [Rgtk4](https://github.com/JanMarvin/Rgtk4) (GTK4).

This package replaces [gWidgets2RGtk2](../gWidgets2RGtk2) for new work. The **gWidgets2** public API stays the same; the GTK implementation underneath is new.

```r
options(guiToolkit = "Rgtk4")
library(gWidgets2)
# once wired: w <- gwindow("Hello"); gbutton("OK", container = w)
```

## Status

Early scaffold. See [docs/](docs/) for plan, architecture, API mapping, and widget inventory.

## Documentation

| Doc | |
|-----|--|
| [docs/plan.md](docs/plan.md) | Phases and success criteria |
| [docs/architecture.md](docs/architecture.md) | Adapter design |
| [docs/api-mapping.md](docs/api-mapping.md) | RGtk2 → Rgtk4 mapping |
| [docs/widget-inventory.md](docs/widget-inventory.md) | Port checklist |
| [docs/inzight-priorities.md](docs/inzight-priorities.md) | iNZight-guided ordering |

## Related packages

- `gWidgets2` — public API
- `Rgtk4` — GTK4 bindings
- `gWidgets2RGtk2` — legacy GTK2 backend (reference implementation)
