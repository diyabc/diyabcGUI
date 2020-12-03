#!/bin/bash

PROJDIR=$(git rev-parse --show-toplevel)
BUILDIR="${PROJDIR}/build"
DISTDIR="${BUILDIR}/dist"

if [[ ! -d $DISTDIR ]]; then mkdir -p $DISTDIR; fi

VERSION=$(R -q -s -e "devtools::load_all(\"${PROJDIR}/R-pkg\", quiet=TRUE); cat(as.character(packageVersion(\"diyabcGUI\")))"|tr -d '\n')

scp WinVM:"Documents/diyabcGUI/build/dist/DIYABC-RF_GUI_${VERSION}_windows.zip" ${DISTDIR}/
