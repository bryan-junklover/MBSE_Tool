# ModelBench

Start with [BUILD_PLAN.md](BUILD_PLAN.md). Using the app is covered in [docs/USER_GUIDE.md](docs/USER_GUIDE.md). The core package is in `modelbench/` and the Shiny shell in `app/`.

```bash
Rscript tools/run_tests.R
```

```bash
Rscript tools/lint.R
```

```bash
Rscript tools/build_sample_project.R survey_uav.sqlite
```

Widgets are local packages, so `renv.lock` leaves them out. Install them after a restore, and after changing one:

```bash
Rscript tools/install_widgets.R
```

```bash
Rscript -e 'shiny::runApp("app")'
```

The ICD export needs Quarto: RStudio bundles it, or set `QUARTO_PATH`. The PDF exports need Google Chrome.
