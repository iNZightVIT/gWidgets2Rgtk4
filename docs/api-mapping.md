# API mapping: RGtk2 / gWidgets2RGtk2 → Rgtk4

Function *names* are often similar (camelCase, RGtk2 style). Call *style* and GTK widget set are not drop-in compatible.

## Call style

| Concern | gWidgets2RGtk2 (RGtk2) | gWidgets2Rgtk4 (Rgtk4) |
|---------|------------------------|-------------------------|
| Construct | `gtkButton()`, `gtkWindow(show=FALSE)` | `gtkButtonNew()`, `gtkButtonNewWithLabel()`, `gtkWindowNew()` |
| Methods | `w$setLabel(x)`, `box$packStart(...)` | `gtkButtonSetLabel(w, x)`, `gtkBoxAppend(box, child)` |
| Signals | `gSignalConnect(...)` | `gSignalConnectR(obj, signal, fun)` |
| Show window | `widget$show()` | `gtkWindowPresent(window)` |
| Close | `"delete-event"` | `"close-request"` |
| Enums | `GtkOrientation["vertical"]` | integers (`0L` horizontal, `1L` vertical) or local constants |
| Properties | `obj["title"] <- ...` | typed setters / `gObjectSet*` / generated Get/Set |
| Type tag | `"RGtkObject"` | still `"RGtkObject"` + Gtk* S3 classes on extptrs |

## Layout (GTK4)

| GTK2 / RGtk2 | GTK4 / Rgtk4 |
|--------------|--------------|
| `gtkHBox` / `gtkVBox` | `gtkBoxNew(orientation, spacing)` |
| `packStart` / `packEnd` | `gtkBoxAppend`, `gtkBoxPrepend`, `gtkBoxInsertChildAfter` |
| `gtkContainerAdd` | `gtkWindowSetChild`, `gtkFrameSetChild`, … |
| `gtkTable` | `gtkGridNew` + `gtkGridAttach` |
| `gtkEventBox` | usually omit; gestures/controllers if needed |

gWidgets2 packing args (`expand`, `fill`, `anchor`) must be mapped onto GTK4 hexpand/vexpand/halign/valign (and box/grid specifics) in helpers — semantics may be approximate where GTK4 has no 1:1.

## Icons

| GTK2 | GTK4 approach |
|------|----------------|
| `gtkStock*`, `GtkIconFactory`, stock ids (`gtk-ok`) | Icon theme names / paintables; maintain a stock→name table in `icons.R` for gWidgets2 stock API |

## Menus, toolbars, status

Classic `GtkMenu` / `GtkMenuBar` / `GtkToolbar` / `GtkStatusbar` patterns from GTK2 do not carry over cleanly. Preserve **gWidgets2 list-based APIs**; implement underneath with GTK4 patterns (`GMenuModel`, `GtkPopoverMenu`, header bar, or a `GtkBox` of buttons) as needed for iNZight.

## Dialogs and files

Prefer Rgtk4 helpers where present:

- `gtkMessageDialogNew` / related
- `gtkDialogRun`, `gtkFileChooserDialogRun` (compat helpers in Rgtk4)

## Graphics

`cairoDevice` + GTK2 drawing does **not** transfer. Current spike: `unigd::ugd()` + PNG blit into `GtkPicture` (see `R/ggraphics.R`). No Cairo R bindings required. Follow-up: httpgd / WebView for web-native plots once iNZight runs on GTK4.

## Events and DnD

| GTK2 | GTK4 |
|------|------|
| `button-press-event` / GdkEvent | `GtkGestureClick` and related controllers |
| `gtkDragSourceSet` / targets | GTK4 `GtkDragSource` / `GtkDropTarget` via `dnd.R` |

Implement DnD only where gWidgets2 exposes it and iNZight needs it; do not block Phase 1 on full DnD parity.

## Reference examples in Rgtk4

Under `Rgtk4/inst/extdata/examples/`:

- `hello_world.R` — window, box, button, signal
- `Confirm_Yes_No.R`, `Confirm_Radio_Buttons.R` — dialogs
- `File_Chooser.R` — file dialogs
- `Top_Menu.R`, `Native_Title_Bar.R` — menus / chrome
- `DataFrameViewer.R` — tables / data frames
- `RightClickMenu.R` — context menus
