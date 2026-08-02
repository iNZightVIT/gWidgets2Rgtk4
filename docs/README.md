# gWidgets2Rgtk4 documentation

Internal design docs for the GTK4 toolkit backend of [gWidgets2](../../gWidgets2).

| Doc | Purpose |
|-----|---------|
| [plan.md](plan.md) | Goals, phases, success criteria |
| [architecture.md](architecture.md) | Adapter pattern, class hierarchy, dispatch |
| [api-mapping.md](api-mapping.md) | RGtk2 → Rgtk4 call-style and GTK4 redesign notes |
| [widget-inventory.md](widget-inventory.md) | Full constructor inventory and port status |
| [inzight-priorities.md](inzight-priorities.md) | What iNZight needs first (post-scaffold) |

Package source lives in `../R/`. These docs are for developers; they are not installed with the package (see `.Rbuildignore`).
