# DIYABC-RF GUI standalone build pipeline

This folder and its contents are only usefull for the standalone app developpers.

If you are a DIYABC-RF GUI user, please refer to README file at project root for installation and use instructions.

## Windows standalone build

> To be run on Windows OS.

1. Build `diyabcGUI` packages zip sources for Windows
```bash
Rscript.exe build_windows_package.R
```

2. Build `DIYABC-RF_GUI` standalone app for Windows
```bash
Rscript.exe build_windows_standalone.R
```

> Release `dist/DIYABC-RF_GUI_<latest_version>.zip`

How to use it: unzip `DIYABC-RF_GUI_<latest_version>.zip` and run `DIYABC-RF_GUI.bat` in the extracted directory (either by double-clicking it or in a terminal).