// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let suppl = it.at("supplement", default: none)
  if suppl == none or suppl == auto {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let target = query(it.target, loc).first()
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}

//#assert(sys.version.at(1) >= 11 or sys.version.at(0) > 0, message: "This template requires Typst Version 0.11.0 or higher. The version of Quarto you are using uses Typst version is " + str(sys.version.at(0)) + "." + str(sys.version.at(1)) + "." + str(sys.version.at(2)) + ". You will need to upgrade to Quarto 1.5 or higher to use apaquarto-typst.")

// counts how many appendixes there are
#let appendixcounter = counter("appendix")
// make latex logo
// https://github.com/typst/typst/discussions/1732#discussioncomment-6566999
#let TeX = style(styles => {
  set text(font: ("New Computer Modern", "Times", "Times New Roman"))
  let e = measure("E", styles)
  let T = "T"
  let E = text(1em, baseline: e.height * 0.31, "E")
  let X = "X"
  box(T + h(-0.15em) + E + h(-0.125em) + X)
})
#let LaTeX = style(styles => {
  set text(font: ("New Computer Modern", "Times", "Times New Roman"))
  let a-size = 0.66em
  let l = measure("L", styles)
  let a = measure(text(a-size, "A"), styles)
  let L = "L"
  let A = box(scale(x: 105%, text(a-size, baseline: a.height - l.height, "A")))
  box(L + h(-a.width * 0.67) + A + h(-a.width * 0.25) + TeX)
})

#let firstlineindent=0.5in

// documentmode: man
#let man(
  title: none,
  runninghead: none,
  margin: (x: 1in, y: 1in),
  paper: "us-letter",
  font: ("Times", "Times New Roman"),
  fontsize: 12pt,
  leading: 18pt,
  spacing: 18pt,
  firstlineindent: 0.5in,
  toc: false,
  lang: "en",
  cols: 1,
  doc,
) = {

  set page(
    paper: paper,
    margin: margin,
    header-ascent: 50%,
    header: grid(
      columns: (9fr, 1fr),
      align(left)[#upper[#runninghead]],
      align(right)[#counter(page).display()]
    )
  )


 
if sys.version.at(1) >= 11 or sys.version.at(0) > 0 {
  set table(    
    stroke: (x, y) => (
        top: if y <= 1 { 0.5pt } else { 0pt },
        bottom: .5pt,
      )
  )
}
  set par(
    justify: false, 
    leading: leading,
    first-line-indent: firstlineindent
  )

  // Also "leading" space between paragraphs
  set block(spacing: spacing, above: spacing, below: spacing)

  set text(
    font: font,
    size: fontsize,
    lang: lang
  )

  show link: set text(blue)

  show quote: set pad(x: 0.5in)
  show quote: set par(leading: leading)
  show quote: set block(spacing: spacing, above: spacing, below: spacing)
  // show LaTeX
  show "TeX": TeX
  show "LaTeX": LaTeX

  // format figure captions
  show figure.where(kind: "quarto-float-fig"): it => [
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #align(center)[#it.body]
  ]
  
  // format table captions
  show figure.where(kind: "quarto-float-tbl"): it => [
    #if int(appendixcounter.display().at(0)) > 0 [
      #heading(level: 2)[#it.supplement #appendixcounter.display("A")#it.counter.display()]
    ] else [
      #heading(level: 2)[#it.supplement #it.counter.display()]
    ]
    #par[#emph[#it.caption.body]]
    #block[#it.body]
  ]

 // Redefine headings up to level 5 
  show heading.where(
    level: 1
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(center)
    #set text(size: fontsize)
    #it.body
  ]
  
  show heading.where(
    level: 2
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize)
    #it.body
  ]
  
  show heading.where(
    level: 3
  ): it => block(width: 100%, below: leading, above: leading)[
    #set align(left)
    #set text(size: fontsize, style: "italic")
    #it.body
  ]

  show heading.where(
    level: 4
  ): it => text(
    size: 1em,
    weight: "bold",
    it.body
  )

  show heading.where(
    level: 5
  ): it => text(
    size: 1em,
    weight: "bold",
    style: "italic",
    it.body
  )

  if cols == 1 {
    doc
  } else {
    columns(cols, gutter: 4%, doc)
  }


}
#show: document => man(
  runninghead: "CORRELATIONAL EVIDENCE FROM GERMANY",
  document,
)


\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Teachers Attitudes Towards Democracy Education as a Function of Their Values
]
)
]
#set align(center)
#block[
\
Samuel Merk#super[1];, Nicolas Hübner#super[2];, and Gabriel Nagy#super[3]

#super[1];Karlsruhe University of Education

#super[2];Eberhard Karls Universität Tübingen

#super[3];Leibniz Institute for Science and Mathematics Education

]
#set align(left)
\
\
#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Author Note
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
Samuel Merk #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) http:\/\/orcid.org/0000-0003-2594-5337

Nicolas Hübner #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) http:\/\/orcid.org/0000-0003-3528-8086

Gabriel Nagy #box(image("_extensions/wjschne/apaquarto/ORCID-iD_icon-vector.svg", width: 4.23mm)) http:\/\/orcid.org/0000-0001-6897-212X

Data collection was funded by Verband Bildung und Erziehung Germany.

Author roles were classified using the Contributor Role Taxonomy (CRediT; https:\/\/credit.niso.org/) as follows: #emph[Samuel Merk];#strong[: ];conceptualization, data curation, formal analysis, funding acquisition, investigation, methodology, validation, visualization, writing original draft, and writing review & editing. #emph[Nicolas Hübner];#strong[: ];conceptualization and writing review & editing. #emph[Gabriel Nagy];#strong[: ];software, formal analysis, methodology, validation, and writing review & editing

Correspondence concerning this article should be addressed to Samuel Merk, Karlsruhe University of Education, Bismarckstraße 10, Karlsruhe, Baden-Württemberg 76133, Germany, Email: merk\@ph-karlsruhe.de

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Abstract
]
)
]
#block[
Alongside professional knowledge, beliefs, motivation and self-regulation, human values are regarded as a central component of teachers’ professional competence. This paper uses a representative sample of $N = 1185$ teachers to describe the prevalence and structure of these values as well as their theoretical function for and empirical association with educational goals specified in the state constitutions. Modelling with the (extended) Circular Stochastic Process Model for the Circumplex shows a theory-compliant quasi-circumplex structure of the values with great heterogeneity in the preferred values and their (intra-individual) discrimination. In contrast, agreement with the institutionally prescribed educational goals shows greater homogeneity, for example in the high level of agreement with the goal of education towards a free democratic basic order. Agreement with the educational goals also covaries with the value profiles in line with the theory. For example, high levels of the values XY predict high levels of agreement with the goal of education for a liberal democratic basic order, while high levels of the values security and tradition predict high levels of agreement with the goal of education for love of one’s homeland.

]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
#emph[Keywords];: Democracy Education, Teacher, Values

#pagebreak()

#block[
#heading(
level: 
1
, 
numbering: 
none
, 
outlined: 
false
, 
[
Teachers Attitudes Towards Democracy Education as a Function of Their Values
]
)
]
#par()[#text(size:0.5em)[#h(0.0em)]]
#v(-18pt)
For decades, if not for centuries, there is a vivid public and academic discussion on "what personal qualities are required for being a good teacher" (#link(<ref-carr2010>)[Carr, 2010, p. 63];). This isn’t surprising as students in OECD countries perceived in 2023 on average 7.634 hours of instruction during their primary and lower secondary education (#link(<ref-oecd2023>)[OECD, 2023];) and—at least in the public opinion—teachers have crucial impact on student learning.Due to this central role in the education system, researchers have spent huge effort in investigating which competencies and characteristics are most predictive for student success (#link(<ref-cochran-smith2005>)[Cochran-Smith & Zeichner, 2005];). But in times of climate change, social divide, rising authoritanism and other crisis (#link(<ref-lawrence2022global>)[Lawrence et al., 2022];) educational goals beyond academic achievement like practicing tolerance or democracy education may get momentum. But which are the most favoured educational goals by teachers and parents and where do they come from? In this brief, we hypothesise educational goals as (among others) functions of human values. These values have beenconceptualized as abstract, and trans-situational ideals or guiding principles Schwartz & Bilsky (#link(<ref-schwartz1987>)[1987];)

This research spans a very broad range from studying cognitive abilities or subject matter knowledge of teachers Roloff Henoch et al. (#link(<ref-roloffhenoch2015>)[2015];) to motivational variables (#link(<ref-richardson2014>)[Richardson et al., 2014];) or personality (#link(<ref-kell2019>)[Kell, 2019];). Thereby at least two different research strands can be identified: First, researchers try to describe the population of teachers based on this characteristic and their interrelations and estimate differences to other occupational or professional groups (#link(<ref-roloffhenoch2015>)[Roloff Henoch et al., 2015];). Second, and mostly in correlational designs, scientists investigate which of these characteristics add which value to students school live and especially learning and third it is investigated how to foster these characteristics by teacher selection, education and professional training. \
Within the second approach This study follows the second approach, but broadens it to a rarely studied characteristic, namely (human) values. As values are studied rather rarely in psychology and educational science (#link(<ref-fries2007>)[Fries et al., 2007];), we first give an introduction to the theory of values (#link(<ref-schwartz1987>)[Schwartz & Bilsky, 1987];) and introduce the literature on the role of teachers values for their professionalism.

= Method
<method>
== Participants
<participants>
== Measures
<measures>
== Procedure
<procedure>
= Results
<results>
= Discussion
<discussion>
== Limitations and Future Directions
<limitations-and-future-directions>
== Conclusion
<conclusion>
= References
<references>
#set par(first-line-indent: 0in, hanging-indent: 0.5in)
#block[
#block[
Carr, D. (2010). Personal and Professional Values in Teaching. In T. Lovat, R. Toomey, & N. Clement (Eds.), #emph[International Research Handbook on Values Education and Student Wellbeing] (pp. 63–74). Springer Netherlands. #link("https://doi.org/10.1007/978-90-481-8675-4_4")

] <ref-carr2010>
#block[
Cochran-Smith, M., & Zeichner, K. M. (Eds.). (2005). #emph[Studying teacher education: The report of the AERA Panel on Research and Teacher Education];. Lawrence Erlbaum Associates.

] <ref-cochran-smith2005>
#block[
Fries, S., Schmid, S., & Hofer, M. (2007). On the relationship between value orientation, valences, and academic achievement. #emph[European Journal of Psychology of Education];, #emph[22];(2), 201–216. #link("https://doi.org/10.1007/BF03173522")

] <ref-fries2007>
#block[
Guarino, C. M., Santibañez, L., & Daley, G. A. (2006). Teacher Recruitment and Retention: A Review of the Recent Empirical Literature. #emph[Review of Educational Research];, #emph[76];(2), 173–208. #link("https://doi.org/10.3102/00346543076002173")

] <ref-guarino2006>
#block[
Kell, H. J. (2019). Do Teachers’ Personality Traits Predict Their Performance? A Comprehensive Review of the Empirical Literature From 1990 to 2018. #emph[ETS Research Report Series];, #emph[2019];(1), 1–27. #link("https://doi.org/10.1002/ets2.12241")

] <ref-kell2019>
#block[
Lawrence, M., Janzwood, S., & Homer-Dixon, T. (2022). What is a global polycrisis. #emph[Cascade Institute, Technical Paper];, #emph[4];.

] <ref-lawrence2022global>
#block[
OECD. (2023). #emph[Education at a Glance 2023: OECD Indicators];. OECD. #link("https://doi.org/10.1787/e13bef63-en")

] <ref-oecd2023>
#block[
Richardson, P. W., Karabenick, S. A., & Watt, H. M. G. (Eds.). (2014). #emph[Teacher motivation: Theory and practice];. Routledge.

] <ref-richardson2014>
#block[
Rokeach, M. (1973). #emph[The nature of human values] (pp. x, 438). Free Press.

] <ref-rokeach1973a>
#block[
Roloff Henoch, J., Klusmann, U., Lüdtke, O., & Trautwein, U. (2015). Who becomes a teacher? Challenging the \"negative selection\" hypothesis. #emph[Learning and Instruction];, #emph[36];, 46–56. #link("https://doi.org/10.1016/j.learninstruc.2014.11.005")

] <ref-roloffhenoch2015>
#block[
Schwartz, S. H., & Bilsky, W. (1987). Toward a universal psychological structure of human values. #emph[Journal of Personality and Social Psychology];, #emph[53];(3), 550–562. #link("https://doi.org/10.1037/0022-3514.53.3.550")

] <ref-schwartz1987>
] <refs>
#set par(first-line-indent: 0.5in, hanging-indent: 0in)
#pagebreak(weak: true)
= Appendix
<appendix>
#counter(figure.where(kind: "quarto-float-fig")).update(0)
#counter(figure.where(kind: "quarto-float-tbl")).update(0)
#appendixcounter.step()
= Title for Appendix
<title-for-appendix>



