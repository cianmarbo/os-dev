ASM=nasm 

BUILD_DIR=build
SRC_DIR=src

.PHONY: all floppy_image kernel bootloader clean always  

#
# Floppy image
#
floppy_image: $(BUILD_DIR)/main_floppy.img 

$(BUILD_DIR)/main_floppy.img: bootloader kernel 
#create empty 1.44mb file
	dd if=/dev/zero of=$(BUILD_DIR)/main_floppy.img bs=512 count=2880

#format file (use mkfs.fat -F 12 -n "NBOS" $(BUILD_DIR)/main floppy.img on linux)
#see https://rubenerd.com/create-new-fat-floppy-disk-image-on-macos/ or https://apple.stackexchange.com/questions/465871/command-to-format-a-raw-floppy-disk-image-with-fat12 for macos
	newfs_msdos -f 2880 -F 12 ./$(BUILD_DIR)/main_floppy.img

#copy bootloader to first 512 bytes (first sector) of image by using dd to copy to image and asking dd not to truncate by setting 'conv=notrunc'
	dd if=$(BUILD_DIR)/bootloader.bin of=$(BUILD_DIR)/main_floppy.img conv=notrunc
#copy kernel to image, this will append the kernel to the rest of the image, "::kernel.bin" is the name of the actual directory in the root (::) of the image where our data will be copied too
	mcopy -i $(BUILD_DIR)/main_floppy.img $(BUILD_DIR)/kernel.bin "::kernel.bin"

#
# Bootloader
#
bootloader: $(BUILD_DIR)/bootloader.bin

$(BUILD_DIR)/bootloader.bin: always
	$(ASM) -f bin -o $(BUILD_DIR)/bootloader.bin $(SRC_DIR)/bootloader/boot.asm

#
#  Kernel
#
kernel: $(BUILD_DIR)/kernel.bin

$(BUILD_DIR)/kernel.bin: always
	$(ASM) -f bin -o $(BUILD_DIR)/kernel.bin $(SRC_DIR)/kernel/main.asm

#
# Always
#
always:
	mkdir -p $(BUILD_DIR)

#
# Clean
#
clean:
	rm -rf $(BUILD_DIR)/* 
