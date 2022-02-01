
# CAVEAT: not really enough to reach fixed point
PDFTrustChain.pdf: *.tex *.bib
	pdflatex -shell-escape PDFTrustChain.tex
	bibtex PdfTrustChain

snapshot:
	mkdir -p $@

snapshot/PDFTrustChain.pdf: snapshot PDFTrustChain.pdf
	cp PDFTrustChain.pdf $@

ghci-spec:
	ghci -x lhs -ietc pre-dom.tex

