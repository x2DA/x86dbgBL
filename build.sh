#!/usr/bin/env bash
SOURCEFOLDER="src/"
TARGETSOURCEFILE="bootloader.s"
OUTPUTIMAGE="bOS.img"

nasm "${SOURCEFOLDER}${TARGETSOURCEFILE}" -f bin -o "${OUTPUTIMAGE}"

