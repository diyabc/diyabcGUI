# DIYABC-RF GUI standalone build pipeline

This folder and its contents are only usefull for the standalone app developpers.

If you are a DIYABC-RF GUI user, please refer to README file at project root for installation and use instructions.

## Windows standalone

### Build

> To be run on Windows OS.

1. Init and/or update `DesktopDeployrR` submodule
```bash
git submodule init
git submodule update
```

2. Build `diyabcGUI` packages zip sources for Windows
```bash
Rscript.exe build_windows_package.R
```

3. Build `DIYABC-RF_GUI` standalone app for Windows
```bash
Rscript.exe build_windows_standalone.R
```

> Release `dist/DIYABC-RF_GUI_<latest_version>.zip`

### Usage

1. Unzip `DIYABC-RF_GUI_<latest_version>.zip` 

2. Run `DIYABC-RF_GUI.bat` in the extracted directory (either by double-clicking it or in a terminal).

> A log file with for DIYABC-RF GUI is available in your user-specific directory for temporary files, generally `C:\Users\<username>\AppData\Local\Temp\DIYABC-RF_GUI.log`.
