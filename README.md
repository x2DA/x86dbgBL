# Space-tuned x86 debugger & demo tool (in bootloader)

## Usage:
### Highlighting:
Cursor: h, j, k, l  
Cursor highlight: v (toggle, highlights from position @ the time of toggle to cursor position)
### Code:
Ideally define variables under `; -- prog_vars --` and instructions under `prog_start`. There is a referenceable `data` label (which gets dumped to screen).
About 256 free bytes.

## Build:
```
nasm "bootloader.s" -f bin -o "NAME.img"
```
## Run:
```
qemu-system-i386 -drive file=NAME.img, format=raw
```
