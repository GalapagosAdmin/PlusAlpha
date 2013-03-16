#!/bin/bash
# Copies po files into application bundle to make OS X happy
mkdir ./PlusAlpha.app/Contents/MacOS/locale
cp ./i18n/*.po ./PlusAlpha.app/Contents/MacOS/locale/
