# Widget inventory

Source of truth for port status. Status values: `todo` | `wip` | `done` | `spike` | `deferred`.

Line counts are from gWidgets2RGtk2 at planning time (~8458 LOC total).

## Infrastructure

| Component | Source (RGtk2 backend) | ~LOC | Difficulty | Status |
|-----------|------------------------|------|------------|--------|
| Package / NAMESPACE | `gWidgets2RGtk2-package.R` | — | Low | done |
| Toolkit class | `misc.R` | 30 | Low | done |
| Startup / init | `startup.R` | 12 | Low | done |
| gtk helpers | `gtk-misc.R` | 146 | Medium | done |
| `GComponent` | `GComponent.R` | 511 | High | done |
| `GContainer` | `GContainer.R` | — | Medium | done |
| `GWidget` | `GWidget.R` | — | Medium | done |
| Icons | `icons.R` | 138 | Medium | done |
| Doc stubs | `aaa.R` | — | Low | done |

## Containers

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gwindow` | `gwindow.R` | 211 | Medium | done | `close-request`, `SetChild`, `Present` |
| `ggroup` / `gvbox` | `ggroup.R` | 167 | Medium | done | `gtkBoxNew` / Append; scroll via `GtkScrolledWindow` |
| `gframe` | `gframe.R` | — | Low | done | `gtkFrameSetChild` |
| `gexpandgroup` | `gexpandgroup.R` | — | Medium | done | `gtkExpander` |
| `glayout` | `glayout.R` | 162 | Medium | done | `GtkGrid` |
| `gformlayout` | `gformlayout.R` | 152 | Medium | done | built on grid |
| `gnotebook` | `gnotebook.R` | 165 | Medium | done | `GtkNotebook` |
| `gpanedgroup` | `gpanedgroup.R` | — | Medium | done | `GtkPaned` |
| `gstackwidget` | `gstackwidget.R` | — | Medium | done | `GtkStack` |

## Controls

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gbutton` | `gbutton.R` | — | Low | done | no EventBox |
| `glabel` | `glabel.R` | — | Low–Med | done | box wrapper; editable entry |
| `gedit` | `gedit.R` | 216 | Medium | done | `GtkEditable` + placeholder |
| `gtext` | `gtext.R` | 213 | Medium | done | `GtkTextView` |
| `gcheckbox` | `gcheckbox.R` | — | Low | done | |
| `gcheckboxgroup` | `gcheckboxgroup.R` | 256 | Medium | done | box of checks |
| `gradio` | `gradio.R` | — | Medium | done | `gtkCheckButtonSetGroup` |
| `gcombobox` | `gcombobox.R` | 249 | High | done | `GtkComboBoxText` |
| `gslider` | `gslider.R` | — | Low | done | `GtkScale` |
| `gspinbutton` | `gspinbutton.R` | — | Low | done | |
| `gprogressbar` | `gprogressbar.R` | — | Low | done | |
| `gseparator` | `gseparator.R` | — | Low | done | |
| `gimage` | `gimage.R` | — | Medium | done | file / icon name |

## Dialogs / misc

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| dialogs | `dialogs.R` | 406 | Medium | done | message/dialog helpers |
| `gfile` / `gfilebrowse` | `gfile.R` | 190 | Medium | done | `gtkFileChooserDialogRun` |
| `gtimer` | `gtimer.R` | — | Low | done | `gTimeoutAdd` |
| `gaction` | `gaction.R` | — | Medium | done | proxy + UI/Gio sync |
| `gcalendar` | `gcalendar.R` | — | Medium | done | Entry + modal GtkCalendar; GDateTime |

## Chrome

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `gmenu` | `gmenu.R` | 156 | Very High | done | GMenuModel + PopoverMenuBar / PopoverMenu; radio→actions, checkbox→stateful |
| `gtoolbar` | `gtoolbar.R` | — | Very High | done | GtkBox of buttons |
| `gstatusbar` | `gstatusbar.R` | — | High | done | GtkStatusbar push/pop |

## Data / trees / graphics

| Constructor | File | ~LOC | Difficulty | Status | GTK4 notes |
|-------------|------|------|------------|--------|------------|
| `ggraphics` | `ggraphics.R` | — | High | spike | unigd + GtkPicture PNG blit; no locator; no further Cairo/httpgd work |
| `gtable` | `gtable.R` | — | High | done | ColumnView; header menus via SetHeaderMenu |
| `gdf` | `gdf.R` | — | Very High | done | EditableLabel; mutate helpers; header menus; undo stack deferred |
| `gtree` | `gtree.R` | — | High | done | ColumnView + TreeListModel + TreeExpander |
| `gvarbrowser` | `gvarbrowser.R` | — | High | done | Incremental category sync; preserve expand/select |

## Cross-cutting

| Concern | Status | Notes |
|---------|--------|-------|
| EventBox wrappers | done | Removed by default |
| Stock icons | done | Compatibility map in `gtk-misc.R` / `icons.R` |
| DnD | done | `dnd.R` + GComponent; gdf column headers via `add_dnd_columns` |
| Font setters | done | CSS via `font<-` / `css<-` / `loadCss`; gtext TextTags |
| expand/fill/anchor | done | Mapped in packing helpers; raw gWidgets `[-1,1]` anchors (9-spot) |
| padding/margin/border | done | CSS padding/border + GTK margin; `set_borderwidth` deprecated → `set_padding` |
| Popup menus | done | PopoverMenu + GestureClick on GComponent |

## Known stubs / gaps

| API | Status |
|-----|--------|
| `addHandlerKeystroke` | stub — warns, does nothing |
| `GComponent$get_size` | stub — always `-1,-1` |
| gdf undo stack | deferred (`can_undo` / `undo` stubs) |
| `ggraphics` locator / rubber-band | not supported |

## Not in this package

Constructors implemented only in gWidgets2 (or other toolkits), e.g. `ghtml`, `gfilter`, `gdfnotebook`, `ggraphicsnotebook` — only add toolkit methods if/when needed.
