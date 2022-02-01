
# CAVEAT: not really enough to reach fixed point

PDFTrustChain.pdf: *.tex *.bib
	bibtex PdfTrustChain
	pdflatex -shell-escape PDFTrustChain.tex

snapshot:
	mkdir -p $@

snapshot/PDFTrustChain.pdf: snapshot PDFTrustChain.pdf
	cp PDFTrustChain.pdf $@

ghci-spec:
	ghci -x lhs -ietc pre-dom.tex

