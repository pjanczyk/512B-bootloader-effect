# Oldschool graphic effect

- works as **x86 bootloader**
- only **512 bytes**
- works in **text mode**
- works in **16-bit** real mode
- uses **custom font**
- uses **custom dynamic 16-color palette**
- written in **assembly**

![Screenshot](https://dl.dropboxusercontent.com/u/14927513/static/github/512B-bootloader-graphic-effect.png)

## Why?
The project had been carried out for a [contest](http://gynvael.coldwind.pl/?id=612), in which it won the second prize.

## How-to

#### Compile
```
nasm source.asm -o bootsector.bin
```

#### Run

- **Bochs Emulator** (2.6.8 or later)
```
bochs -q -f bochs-config.bxrc
```

- **Virtual machine** - create 1.44 MB floppy image
```
dd if=/dev/zero of=floppy.img bs=512 count=2880
dd if=bootsector.bin of=floppy.img bs=512 count=1 conv=notrunc
```

- **Real PC** - copy bootsector to floppy disk
```
dd if=bootsector.bin of=/dev/fd0 bs=512 count=1
```
