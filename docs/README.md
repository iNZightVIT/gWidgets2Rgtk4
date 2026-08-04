# gWidgets2Rgtk4 documentation

Internal design docs for the GTK4 toolkit backend of [gWidgets2](../../gWidgets2).

| Doc | Purpose |
|-----|---------|
| [architecture.md](architecture.md) | Adapter pattern, discovery, class hierarchy, design rules |
| [api-mapping.md](api-mapping.md) | RGtk2 → Rgtk4 call style and GTK4 implementation notes |
| [widget-inventory.md](widget-inventory.md) | Constructor inventory and port status |

Package source lives in `../R/`. These docs are for developers; they are not installed with the package (see `.Rbuildignore`).

For iNZight migration gaps (toolkit leaks, helpers still needed, app rewrites), see the monorepo root [`inzight-gtk-gaps.md`](../../inzight-gtk-gaps.md).
