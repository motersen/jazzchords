\version "2.20.0"
$(load "../jazzchords.scm")

\layout {
  indent = #0
  ragged-right = ##f
}

\paper {
  oddFooterMarkup = ##f
}

\score {
  \new Staff {
    \clef "bass"
    \relative {
      c8
      \jc "C7/E" e,4.
      \jc "FMaj7" f4
      \jc "F#0" fis
      \jazz "G7sus4" g8
      a
      \jc "G7/B" b
      \jc "C6/9" c~
      c2
    }
  }
}
\markup {
  \fill-line {
    \column {
      \line { "Or try something more elaborate:"
	      \hspace #3
	      \jazzchord "Eb+Maj7#9"
	      \hspace #3
	      \jazzchord "F13b9#11/G"
	      \hspace #3
	      \jazzchord "Gm7|Fm7/Bb"
	    }
      \vspace #1
      \line { "The parser understands chord symbol structure, so it can display (almost) any chord you would think of." }
    }
  }
}
