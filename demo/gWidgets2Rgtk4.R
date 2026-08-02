## Smoke-load the Rgtk4 toolkit backend for gWidgets2.
## Full widget demos arrive with Phase 1 constructors.
require(gWidgets2)
options(guiToolkit = "Rgtk4")

tk <- guiToolkit("Rgtk4")
stopifnot(is(tk, "guiWidgetsToolkitRgtk4"))
message("guiToolkit Rgtk4 ready (", class(tk)[1], ").")

ex <- system.file("examples", "run_examples.R", package = "gWidgets2")
if (nzchar(ex) && interactive()) {
  ## Only useful once Phase 1 widgets are implemented.
  message("To exercise gWidgets2 examples after Phase 1:\n  source(\"", ex, "\")")
}
