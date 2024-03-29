%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This class file defines a style useful for writing white papers at Galois.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\ProvidesClass{galois-whitepaper}
\DeclareOption*{% 
\PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass{article}

% Useful packages:
\RequirePackage[table]{xcolor}
\RequirePackage{graphicx}
\RequirePackage[headheight=0.8in,headsep=0.1in,top=1in,left=1in,right=1in,bottom=1in]{geometry}
\RequirePackage{lastpage}
\RequirePackage{xspace}

% Define fonts
\RequirePackage[sc]{mathpazo}
\linespread{1.0}
\RequirePackage[scaled]{helvet}
\RequirePackage{sectsty}
\allsectionsfont{\sffamily}

\newcommand{\highlight}[2][yellow]{\colorbox{#1}{#2}}

% Misc defines
\newcommand{\@companyName}{\vspace*{.3cm} Galois, Inc.}
\newcommand{\@addressPhone}{
 421 SW 6th Ave., Ste.\ 300\\
 Portland, OR 97204
}
\newcommand{\@contact}{CONTACT}
\newcommand{\@subtitle}{SUBTITLE}

\newcommand{\contact}[1]{\renewcommand{\@contact}{{#1}}}

\renewcommand{\paragraph}[1]{\textbf{\sffamily{#1}}\quad}

\definecolor{galoislightgray}{gray}{0.6}

% Define header and footer
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyheadoffset[l]{1in}
\fancyheadoffset[r]{0in}
%\fancyheadoffset[r]{0.2in}
\fancyhead{}%clear
\fancyfoot{}%clear
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{1pt}
\setlength{\parskip}{11pt} % TODO!


% Define a special header style for the title page:
\newcommand{\ps@title}{%
  \renewcommand{\@oddhead}{%
    \sffamily\hspace{-0.25in}\includegraphics[width=1in]{figures/galois.png}}
    }

% Set up the headers/footers for the rest of the document:
\lhead{\sffamily \hspace{0.75in}\includegraphics[width=1in]{figures/galois.png}}

\rhead{
  \sffamily \vspace{.2in}
  \textit{\@title{}}\\
  \textit{MONTH XX, 2020{}}\\
  \color{galoislightgray}
}

\lfoot{\sffamily\color{galoislightgray}\small\copyright{} 2022 Galois, Inc.}

\cfoot{\sffamily\color{galoislightgray}\small {} Proprietary}

\rfoot{
  \sffamily \color{galoislightgray}\small
  Page \thepage{}% of \pageref{LastPage}
}

% The title content definition:
\DeclareRobustCommand{\maketitle}{
    \thispagestyle{title}
      {\sffamily
        {\large \textbf{}}\\
        \textbf{\sffamily \large \@title}\\
        {\color{galoislightgray} \textit{Dr.\ INSERT\footnotemark[1], Dr.\ INSERT\footnotemark[1]Dr.\ SUB INSERT\footnotemark[3]} }\\
        {\color{galoislightgray} \textit{\footnotemark[1] Galois, Inc.}\\
            \textit{\footnotemark[2] SUBCONTRACTOR}\\
            \texttt{INSERT@galois.com} }\\
        {\color{galoislightgray} \textit{MONTH XX, 2021} }\\
      }
}

% Add a stop ``.'' after each section number:
\renewcommand*{\@seccntformat}[1]{%
  \csname the#1\endcsname.\quad
}

% Redefine the bibliography environment to use \section{\bibname}
% instead of \section*{\bibname}
\makeatletter
\newcommand{\bibname}{References}
\renewenvironment{thebibliography}[1]
     {\section*{\bibname}% <-- this line was changed from \chapter* to \section*
      \@mkboth{\MakeUppercase\bibname}{\MakeUppercase\bibname}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

% Adjust spacing around headings:
\renewcommand\section{\@startsection {section}{1}{\z@}%
  {-0ex \@plus -1ex \@minus -.2ex}%
%  {0.2ex \@plus.1ex}%
  {0.1ex \@plus .1ex \@minus -.1ex}%
  {\raggedright\sffamily\large\bfseries}}

\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
  {-0ex\@plus -1ex \@minus -.2ex}%
  {0.1ex \@plus .1ex \@minus -.1ex}%
%  {-1em}%
  {\raggedright\sffamily\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
  {-0ex \@plus -1ex \@minus -.2ex}%
  {0.1ex \@plus .1ex \@minus -.1ex}%
%  {-1em}%
  {\sffamily\normalsize\bfseries}} %

\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
  {-0ex \@plus -1ex \@minus -.2ex}%
  {-1em}%
  {\normalsize\bfseries}} %

\makeatother