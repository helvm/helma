#!/bin/bash

# Script to recursively convert PNG images to PPM (P3 - ASCII format)
# Usage: ./convert_png_to_ppm.sh [folder]
# Default: current folder (.)

TARGET_DIR="${1:-.}"
CONVERTED_COUNT=0
ERROR_COUNT=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}=== PNG to PPM (P3 - ASCII) Conversion ===${NC}"
echo "Target directory: $TARGET_DIR"
echo ""

# Check if directory exists
if [ ! -d "$TARGET_DIR" ]; then
    echo -e "${RED}Error: Directory '$TARGET_DIR' does not exist${NC}"
    exit 1
fi

# Check if ImageMagick is installed
if ! command -v convert &> /dev/null; then
    echo -e "${RED}Error: ImageMagick is not installed${NC}"
    echo "Install it with: sudo apt-get install imagemagick"
    exit 1
fi

# Find and convert all PNG files recursively
while IFS= read -r -d '' png_file; do
    # Output file path (PNG → PPM)
    ppm_file="${png_file%.png}.ppm"

    echo -n "Converting: $png_file ... "

    # Conversion with error handling
    if convert "$png_file" -depth 8 "$ppm_file" 2>/dev/null; then
        echo -e "${GREEN}✓ OK${NC}"
        ((CONVERTED_COUNT++))
    else
        echo -e "${RED}✗ ERROR${NC}"
        ((ERROR_COUNT++))
    fi
done < <(find "$TARGET_DIR" -type f -iname "*.png" -print0)

# Summary
echo ""
echo -e "${YELLOW}=== Summary ===${NC}"
echo -e "Converted: ${GREEN}$CONVERTED_COUNT${NC}"
echo -e "Errors: ${RED}$ERROR_COUNT${NC}"

if [ $ERROR_COUNT -eq 0 ] && [ $CONVERTED_COUNT -gt 0 ]; then
    echo -e "${GREEN}All done! ✓${NC}"
    exit 0
elif [ $CONVERTED_COUNT -eq 0 ]; then
    echo -e "${YELLOW}No PNG files found${NC}"
    exit 1
else
    exit 1
fi
