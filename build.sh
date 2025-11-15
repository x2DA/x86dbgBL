#!/usr/bin/env bash
SOURCEFOLDER="src/"
CACHEFOLDER="cache/"
BUILDFOLDER="./"

OUTPUTIMAGE="os.img"

# For intermediary files
INFORMATEXT="s"
OUTFORMATEXT="bin"


# $1 filename
buildsourcefile() {
	if [[ -z "$1" ]]; then
		echo "Parameter not specified."
		exit
	fi

	if [ ! -f "${SOURCEFOLDER}$1" ]; then
		echo "Error: ${SOURCEFOLDER}$1 does not exist."
	fi
	
	BASENAME="${1%.*}"
	nasm "${SOURCEFOLDER}$1" -f bin -o "${CACHEFOLDER}${BASENAME}.${OUTFORMATEXT}"
} 

# $1 base filename
cachetoout() {
	if [[ -z "$1" ]]; then
		echo "Parameter not specified"
		exit
	fi

	cat "${CACHEFOLDER}$1.${OUTFORMATEXT}" >> "${BUILDFOLDER}${OUTPUTIMAGE}"
}

# ----
# main
# ----

if [[ ! -d ${SOURCEFOLDER} ]] || [[ ! -d ${CACHEFOLDER} ]] || [[ ! -d ${BUILDFOLDER} ]]; then
	echo "Error: source, cache or build folder does not exist."
	exit
fi


# Prepare output file
touch "${OUTPUTIMAGE}"
> "${OUTPUTIMAGE}"

# Build output image
buildsourcefile "bootloader.s"
cachetoout "bootloader"

buildsourcefile "kernel.s"
cachetoout "kernel"

