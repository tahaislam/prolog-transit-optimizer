#!/bin/bash
# Generate PDFs for mobile/tablet reading

echo "Generating PDFs from markdown files..."

# Check if pandoc is installed
if ! command -v pandoc &> /dev/null; then
    echo "Error: pandoc is not installed"
    echo "Install with: sudo apt-get install pandoc texlive-latex-base"
    exit 1
fi

# Create output directory
mkdir -p pdfs

# Generate PDFs
echo "Converting README.md..."
pandoc README.md -o pdfs/README.pdf --toc --toc-depth=3 \
    --metadata title="Prolog Transit Route Optimizer"

echo "Converting QUICKSTART.md..."
pandoc QUICKSTART.md -o pdfs/QUICKSTART.pdf --toc --toc-depth=2 \
    --metadata title="Quick Start Guide"

echo "Converting learning-path.md..."
pandoc session_notes/learning-path.md -o pdfs/learning-path.pdf --toc --toc-depth=3 \
    --metadata title="Prolog Learning Path"

echo "Converting prolog-quick-reference.md..."
pandoc session_notes/prolog-quick-reference.md -o pdfs/prolog-quick-reference.pdf \
    --metadata title="Prolog Quick Reference"

echo "Converting mobile-reading-options.md..."
pandoc session_notes/mobile-reading-options.md -o pdfs/mobile-reading-options.pdf \
    --metadata title="Mobile Reading Options"

echo ""
echo "✓ PDFs generated in pdfs/ directory:"
ls -lh pdfs/

echo ""
echo "Transfer these to your phone/tablet via:"
echo "  - Google Drive: Copy pdfs/ folder to Drive"
echo "  - USB: Connect device and copy pdfs/ folder"
echo "  - Email: Attach PDFs and email to yourself"
echo "  - Cloud: Dropbox, OneDrive, etc."
