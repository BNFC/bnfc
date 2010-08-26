--1 A Small Romance Resource Syntax
--
-- Aarne Ranta 2002
--
-- This resource grammar contains definitions needed to construct 
-- indicative, interrogative, and imperative sentences in Romance languages.
-- We try to share as much as possible. Even if the definitions of certain
-- operations are different in $syntax.Fra.gf$ and $syntax.Ita.gf$, we can
-- often give their type signatures in this file.
--
-- The following files are presupposed:

interface SyntaxRomance = TypesRomance ** open Prelude, (CO=Coordination) in {

--2 Common Nouns
--
-- Common nouns are defined as number-dependent strings with a gender.
-- Complex common noun ($CommNounPhrase$)  have the same type as simple ones.
-- (The distinction is made just because of uniformity with other languages.)

oper
  CommNoun : Type = {s : Number => Str ; g : Gender} ;
  CommNounPhrase = CommNoun ;
  noun2CommNounPhrase : CommNounPhrase -> CommNoun = \x -> x ;

  commonNounComp : CommNoun -> Str -> CommNoun = \numero, detelephone -> 
    {s = \\n => numero.s ! n ++ detelephone ;
     g = numero.g
    } ;


--2 Noun phrase
--
-- The worst case is pronouns, which have inflection in the possessive
-- forms. Other noun phrases express all possessive forms with the genitive case.
-- Proper names are the simples example.

  ProperName : Type = {s : Str ; g : Gender} ;

  NounPhrase : Type = Pronoun ; -- the worst case

  nameNounPhrase : ProperName -> NounPhrase ;

  mkProperName : Str -> Gender -> ProperName = \jean,m ->
    {s = jean ; g = m} ;

  mkNameNounPhrase : Str -> Gender -> NounPhrase = \jean,m ->
    nameNounPhrase (mkProperName jean m) ;

  nounPhraseOn : NounPhrase ; 

  normalNounPhrase : (CaseA => Str) -> Gender -> Number -> NounPhrase = \cs,g,n ->
    {s = \\p => cs ! (pform2case p) ;
     g = PGen g ;
     n = n ;
     p = P3 ;    -- third person
     c = Clit0   -- not clitic
    } ;

  pronNounPhrase : Pronoun -> NounPhrase = \pro -> pro ;

-- Many determiners can be modified with numerals, which may be inflected in
-- gender. The label $isNo$ is a hack used to force $des$ for plural
-- indefinite with $noNum$.

  Numeral : Type = {s : Gender => Str ; n : Number ; isNo : Bool} ;

  pronWithNum : Pronoun -> Numeral -> Pronoun = \nous,deux ->
    {s = \\c => nous.s ! c ++ deux.s ! pgen2gen nous.g ; 
     g = nous.g ; 
     n = nous.n ;
     p = nous.p ;
     c = nous.c
    } ;

  noNum : Numeral = {s = \\_ => [] ; n = Pl ; isNo = True} ;

-- The existence construction "il y a", "c'è / ci sono" is defined separately,
-- and ad hoc, in each language.

  existNounPhrase : NounPhrase -> Clause ;

-- To add a symbol, such as a variable or variable list, to the end of
-- an NP.

  addSymbNounPhrase : NounPhrase -> Str -> NounPhrase = \np,x ->
    {s = \\c => np.s ! c ++ x ;
     g = np.g ;
     n = np.n ;
     p = np.p ;
     c = np.c
    } ;


--2 Determiners
--
-- Determiners are inflected according to the gender of the nouns they determine.
-- The determiner determines the number of the argument noun.

  Determiner : Type = {s : Gender => Str ; n : Number} ;
  DeterminerNum : Type = {s : Gender => Str} ;

  detNounPhrase : Determiner -> CommNoun -> NounPhrase = \tout, homme -> 
    normalNounPhrase
      (\\c => prepCase c ++ tout.s ! homme.g ++ homme.s ! tout.n)
      homme.g 
      tout.n ;


  numDetNounPhrase : DeterminerNum -> Numeral -> CommNounPhrase -> NounPhrase = 
    \tous, six, homme -> 
    normalNounPhrase
      (\\c => prepCase c ++ tous.s ! homme.g ++ six.s ! homme.g ++ homme.s ! six.n)
      homme.g 
      six.n ;

--- Here one would like to provide a feminine variant as well.

  justNumDetNounPhrase : DeterminerNum -> Numeral -> NounPhrase = 
    \tous, six -> 
    normalNounPhrase
      (\\c => prepCase c ++ tous.s ! Masc ++ six.s ! Masc)
      Masc
      six.n ;

-- The following macros are sufficient to define most determiners,
-- as shown by the examples that follow.

  mkDeterminer : Number -> Str -> Str -> Determiner = \n,tous,toutes -> 
    {s = genForms tous toutes ; n = n} ;

  mkDeterminer1 : Number -> Str -> Determiner = \n,chaque -> 
    mkDeterminer n chaque chaque ;

  mkDeterminerNum : Str -> Str -> DeterminerNum = 
    \tous,toutes -> 
    {s = \\g => genForms tous toutes ! g} ;


-- Indefinite and definite noun phrases are treated separately,
-- since noun phrases formed by them also depend on case.
-- The indefinite case with a numeral has no separate article:
-- "il y a 86 voitures", not "il y a des 86 voitures".

  indefNounPhrase : Number -> CommNounPhrase -> NounPhrase = \n,mec -> 
    normalNounPhrase 
      (\\c => artIndef mec.g n c ++ mec.s ! n)
      mec.g 
      n ; 

  indefNounPhraseNum : Numeral -> CommNounPhrase -> NounPhrase = \nu,mec -> 
    normalNounPhrase 
      (\\c => case nu.isNo of {
                True => artIndef mec.g Pl c ++ mec.s ! Pl ;
                _ => prepCase c ++ nu.s ! mec.g ++ mec.s ! nu.n
                }
      )
      mec.g 
      nu.n ; 

  defNounPhrase : Number -> CommNounPhrase -> NounPhrase = \n,mec -> 
    normalNounPhrase 
      (\\c => artDef mec.g n c ++ mec.s ! n)
      mec.g
      n ; 

  defNounPhraseNum : Numeral -> CommNounPhrase -> NounPhrase = \nu,mec -> 
    normalNounPhrase 
      (\\c => artDef mec.g nu.n c ++ nu.s !mec.g ++ mec.s ! nu.n)
      mec.g
      nu.n ; 

-- We often need indefinite noun phrases synacategorematically.

  indefNoun : Number -> CommNounPhrase -> Str = \n,mec -> 
    (indefNounPhrase n mec).s ! case2pform nominative ;

-- Genitives of noun phrases can be used like determiners, to build noun phrases.
-- The number argument makes the difference between "ma maison" - "mes maisons".
-- The clitic type of the NP decides between "ma maison" and "la maison de Jean".

  npGenDet : Number -> NounPhrase -> CommNounPhrase -> NounPhrase = 
    \n,jeanne,mec ->
    let {str : CaseA => Str = case jeanne.c of {
          Clit0 => npGenDe   n jeanne mec ;
          _     => npGenPoss n jeanne mec
          }
        } in
    normalNounPhrase str mec.g n ; 

  npGenDetNum : Numeral -> NounPhrase -> CommNounPhrase -> NounPhrase = 
    \nu,jeanne,mec ->
    let {str : CaseA => Str = case jeanne.c of {
          Clit0 => npGenDeNum   nu jeanne mec ;
          _     => npGenPossNum nu jeanne mec
          }
        } in
    normalNounPhrase str mec.g nu.n ; 

-- These auxiliary rules define the genitive with "de" and with the possessive.
-- Here there is a difference between French and Italian: Italian has a definite
-- article before possessives (with certain exceptions).

  npGenDe : Number -> NounPhrase -> CommNounPhrase -> CaseA => Str = 
    \n,jeanne,mec ->
    \\c => artDef mec.g n c ++ mec.s ! n ++ jeanne.s ! case2pform genitive ;

  npGenDeNum : Numeral -> NounPhrase -> CommNounPhrase -> CaseA => Str = 
    \nu,jeanne,mec ->
    \\c => artDef mec.g nu.n c ++ nu.s ! mec.g ++ mec.s ! nu.n ++ 
           jeanne.s ! case2pform genitive ;

  npGenPoss : Number -> NounPhrase -> CommNounPhrase -> CaseA => Str ;

  npGenPossNum : Numeral -> NounPhrase -> CommNounPhrase -> CaseA => Str ;

-- Constructions like "l'idée que la terre est ronde" are formed at the
-- first place as common nouns, so that one can also have "la suggestion que...".

  nounThatSentence : CommNounPhrase -> Sentence -> CommNounPhrase = \idee,x -> 
    {s = \\n => idee.s ! n ++ elisQue ++ x.s ! Ind ; 
     g = idee.g
    } ;


-- The partitive noun phrase has special nominative and accusative, which look like
-- genitives ("du vin, avec du vin", as well as genitive form, where the definite
-- article contracts away ("de vin").

  partitiveNounPhrase : Number -> CommNounPhrase -> NounPhrase ;


--2 Adjectives
--
-- Adjectives have a parameter $p$ telling if postposition is 
-- allowed (complex APs). There is no real need in Romance languages to distinguish
-- between simple adjectives and adjectival phrases.

  Adjective : Type = Adj ** {p : Bool} ;

  adjPre = True ; adjPost = False ;
  
  AdjPhrase : Type = Adjective ;
  
  adj2adjPhrase : Adjective -> AdjPhrase = \x -> x ;

  mkAdjective : Adj -> Bool -> Adjective = \adj,p -> adj ** {p = p} ;


--3 Comparison adjectives
--
-- The type is defined in $TypesRomance$. Syntax adds to lexicon the position
-- information.

  AdjDegr = AdjComp  ** {p : Bool} ;

  mkAdjDegr : AdjComp -> Bool -> AdjDegr = \adj,p ->
    adj ** {p = p} ;

  mkAdjDegrLong : Adj -> Bool -> AdjDegr = \adj,p ->
    adjCompLong adj ** {p = p} ;


-- Each of the comparison forms has a characteristic use:
--
-- Positive forms are used alone, as adjectival phrases ("bon").

  positAdjPhrase : AdjDegr -> AdjPhrase = \bon -> 
    {s = bon.s ! Pos ; 
     p = bon.p
    } ;

-- Comparative forms are used with an object of comparison, as
-- adjectival phrases ("meilleur que toi"). The comparing conjunction
-- is of course language-dependent; Italian moreover has the free
-- variants "che" and "di".

  comparAdjPhrase : AdjDegr -> NounPhrase -> AdjPhrase = \bon, toi ->
    {s = \\a => bon.s ! Comp ! a ++ comparConj ++ 
         toi.s ! stressed accusative ; 
     p = False
    } ;

  comparConj : Str ;

-- Superlative forms are used with a common noun, picking out the
-- maximal representative of a domain 
-- ("le meilleur mec", "le mec le plus intelligent").

  superlNounPhrase : AdjDegr -> CommNoun -> NounPhrase = \bon, mec ->
    normalNounPhrase 
      (\\c => artDef mec.g Sg c ++ if_then_else Str bon.p 
           (bon.s ! Comp ! AF mec.g Sg ++ mec.s ! Sg)
           (mec.s ! Sg ++ artDef mec.g Sg nominative ++ bon.s ! Comp ! AF mec.g Sg)
      )
      mec.g 
      Sg ; 

  superlAdjPhrase : AdjDegr -> AdjPhrase = \bon ->
    {s = \\a => artDef (genAForm a) (numAForm a) nominative ++ bon.s ! Comp ! a ; 
     p = bon.p
    } ;

-- Sentence-complement adjectives.
---- Need proper mode in the negative case.

  predAdjSent : (Adjective ** {mp,mn : Mode}) -> Sentence -> Clause = 
    \adj,ildort ->
    sats2clause (
      insertExtrapos (mkSatsCopula pronImpers (adj.s ! AF Masc Sg)) 
        (\\b => embedConj ++ ildort.s ! if_then_else Mode b adj.mp adj.mn)) ;

  predAdjSent2 : (AdjCompl ** {mp,mn : Mode}) -> NounPhrase -> 
   ( Adjective ** {mp,mn : Mode}) = \facile,jean ->
    complAdj facile jean ** {mp = facile.mp ; mn = facile.mn} ;

  pronImpers : NounPhrase ;

-- $pronImpers = pronNounPhrase pronIl$ in French, empty in Italian
-- and Spanish.

--3 Prepositions and complements
--
-- Most prepositions are just strings. But "à" and "de" are treated as cases in
-- French. In Italian, there are more prepositions treated in this way:
-- "a", "di", "da", "in", "su", "con".
-- An invariant is that, if the preposition is not empty ($[]$), then the case
-- is $Acc$.

  Preposition = Str ;

  Complement = {s2 : Preposition ; c : CaseA} ;

  complement : Str -> Complement = \par ->
    {s2 = par ; c = nominative} ;

  complementDir : Complement = complement [] ;

  complementCas : CaseA -> Complement = \c ->
    {s2 = [] ; c = c} ;


--3 Two-place adjectives
--
-- A two-place adjective is an adjective with a preposition used before
-- the complement, and the complement case.

  AdjCompl = AdjPhrase ** Complement ; 

  mkAdjCompl : Adj -> Bool -> Complement -> AdjCompl = \adj,p,c ->
    mkAdjective adj p ** c ;

  complAdj : AdjCompl -> NounPhrase -> AdjPhrase = \relie,jean ->
    {s = \\a => relie.s ! a ++ relie.s2 ++ jean.s ! case2pform relie.c ; 
     p = False
    } ;


--3 Modification of common nouns
--
-- The two main functions of adjective are in predication ("Jean est jeune")
-- and in modification ("un jeune homme"). Predication will be defined
-- later, in the chapter on verbs.
--
-- Modification must pay attention to pre- and post-noun
-- adjectives: "jeune homme"; "homme intelligent".

  modCommNounPhrase : AdjPhrase -> CommNounPhrase -> CommNounPhrase = \bon,mec -> 
    {s = \\n => if_then_else Str bon.p 
           (bon.s ! AF mec.g n ++ mec.s ! n)
           (mec.s ! n ++ bon.s ! AF mec.g n) ;
     g = mec.g
    } ;

--2 Function expressions

-- A function expression is a common noun together with the
-- preposition prefixed to its argument ("mère de x").
-- The type is analogous to two-place adjectives and transitive verbs.

  Function : Type = CommNounPhrase ** Complement ;

-- The application of a function gives, in the first place, a common noun:
-- "mor/mödrar till Johan". From this, other rules of the resource grammar 
-- give noun phrases, such as "la mère de Jean", "les mères de Jean",
-- "les mères de Jean et de Marie", and "la mère de Jean et de Marie" (the
-- latter two corresponding to distributive and collective functions,
-- respectively). Semantics will eventually tell when each
-- of the readings is meaningful.

  appFunComm : Function -> NounPhrase -> CommNounPhrase = \mere,jean -> 
    {s = \\n => mere.s ! n ++ mere.s2 ++ jean.s ! case2pform mere.c ;
     g = mere.g
    } ;

-- Two-place functions add one argument place.

  Function2 = Function ** {s3 : Preposition ; c3 : CaseA} ;

-- There application starts by filling the first place.

  appFun2 : Function2 -> NounPhrase -> Function = \vol, paris ->
    {s = \\n => vol.s ! n ++ vol.s2 ++ paris.s ! case2pform vol.c ;
     g = vol.g ;
     s2 = vol.s3 ;
     c = vol.c3
    } ;


-- It is possible to use a function word as a common noun; the semantics is
-- often existential or indexical.

  funAsCommNounPhrase : Function -> CommNounPhrase = \x -> x ; 

-- The following is an aggregate corresponding to the original function application
-- producing "ma mère" and "la mère de Jean". It does not appear in the
-- resource grammar API any longer.

  appFun : Bool -> Function -> NounPhrase -> NounPhrase = \coll, mere, jean ->
    let 
      n = jean.n ; 
      g = mere.g ; nf = if_then_else Number coll Sg n
    in variants {
      defNounPhrase nf (appFunComm mere jean) ;
      npGenDet nf jean mere
      } ;


--2 Verbs
--
--3 Verb phrases
--
-- Unlike many other languages, verb phrases in Romance languages
-- are not discontinuous.
-- We use clitic parameters instead. 
--
-- (It is not quite sure, though, whether this
-- will suffice in French for examples like "je n'*y* vais pas": one may want to
-- add "y" to "ne vais pas" instead of "ne - pas" to "y vais".)

param
  VPForm = VPF Anteriority VF ;
  Anteriority = Simul | Anter ;
  VIForm = VIInfinit | VIImperat Bool Number | VIGerund ;

oper
  VerbPhrase = {s : VIForm => Gender => Number => Person => Str} ;
  VerbClause = {s : Bool => Anteriority => VIForm => Gender => Number => Person => Str} ;

  vpf2vf : VPForm -> VF = \vpf -> case vpf of {
    VPF _ vf => vf
    } ;

  auxVerb : Verb -> Verb ; -- gives the auxiliary

  nombreVerbPhrase : VPForm -> Number = \v -> case v of {
    VPF _ f => nombreVerb f
    } ;

  personVerbPhrase : VPForm -> Person = \v -> case v of {
    VPF _ f => personVerb f
    } ;

  isNotImperative : VPForm -> Bool = \v -> case v of {
    VPF _ (VImper _) => False ;
    _ => True
    } ;

-- Predication is language-dependent in the negative case.

  negVerb : Str -> Str ;

-- Verb phrases can also be formed from adjectives ("est bon"),
-- common nouns ("est un homme"), and noun phrases ("est Jean").
-- We need a copula, which is of course language-dependent.

  copula : Verb ;

-- The third rule is overgenerating: "est chaque homme" has to be ruled out
-- on semantic grounds.

  complVerbAdj : AdjCompl -> VerbPhrase -> AdjPhrase = \facile,ouvrir ->
    {s = \\gn => ---- p
      facile.s ! gn ++ prepCase facile.c ++ facile.s2 ++
      ouvrir.s ! VIInfinit ! Masc ! Sg ! P3 ;
     p = False
    } ;

  complVerbAdj2 : Bool -> AdjCompl -> NounPhrase -> VerbPhrase -> AdjPhrase = 
    \b,facile,lui,nager ->
    {s = \\gn => ---- p 
        facile.s ! gn ++ 
        lui.s ! stressed dative ++         ---- also "pour lui" ?
        prepCase facile.c ++ facile.s2 ++
        nager.s ! VIInfinit ! pgen2gen lui.g ! lui.n ! P3 ; ---- agr dep on b
     p = False
     } ;

-- complement a verb with noun phrase and optional preposition

  TransVerb : Type = Verb ** Complement ;

  complementOfTransVerb : TransVerb -> Complement = \v -> {s2 = v.s2 ; c = v.c} ;



  verbOfTransVerb : TransVerb -> Verb = \v -> 
    {s = v.s ; aux = v.aux} ;

  isNounPhraseClit : NounPhrase -> Bool = \n -> case n.c of {
     Clit0 => False ;
     _  => True
     } ;

-- This function is language-dependent, because it uses the language-dependent
-- type of case.

  isClitCase : CaseA -> Bool ;

  isTransVerbClit : TransVerb -> Bool = \v -> isClitCase v.c ;

  isDitransVerbClit : DitransVerb -> Bool * Bool = \v -> 
    <isClitCase v.c,isClitCase v.c3> ;

--3 Transitive verbs
--
-- Transitive verbs are verbs with a preposition for the complement,
-- in analogy with two-place adjectives and functions.
-- One might prefer to use the term "2-place verb", since
-- "transitive" traditionally means that the inherent preposition is empty.
-- Such a verb is one with a *direct object* - which may still be accusative,
-- dative, or genitive.
--
-- In complementation, we do need some dispatching of clitic types:
-- "aime Jean" ; "n'aime pas Jean" ; "l'aime" ; "ne l'aime pas".
-- More will be needed when we add ditransitive verbs.

  reflPron : Number => Person => NPFormA => Str ;

  reflPronNounPhrase : Gender -> Number -> Person -> NounPhrase = \g,n,p -> 
    {s = reflPron ! n ! p ;
     g = PGen g ;  -- needed in participle agreement
     n = n ;
     p = p ;
     c = Clit1 ---- depends on person?
    } ;

---- There is no adverbial form for the past participle.

  adjPastPart : Verb -> Adjective = \verb -> {
    s = table {
      AF g n => verb.s ! VPart g n ;
      AA => verb.s ! VPart Masc Sg
      } ;
    p = False
    } ;

  mkTransVerb : Verb -> Preposition -> CaseA -> TransVerb = \v,p,c -> 
    v ** {s2 = p ; c = c} ;

  mkTransVerbPrep : Verb -> Preposition -> TransVerb = \passer,par -> 
    mkTransVerb passer par accusative ;

  mkTransVerbCas : Verb -> CaseA -> TransVerb = \penser,a -> 
    mkTransVerb penser [] a ;

  mkTransVerbDir : Verb -> TransVerb = \aimer -> 
    mkTransVerbCas aimer accusative ;

-- Transitive verbs can be used elliptically as verbs. The semantics
-- is left to applications. The definition is trivial, due to record
-- subtyping.

  transAsVerb : TransVerb -> Verb = \love -> 
    love ;

-- *Ditransitive verbs* are verbs with three argument places.
-- We treat so far only the rule in which the ditransitive
-- verb takes both complements to form a verb phrase.

  DitransVerb = TransVerb ** {s3 : Preposition ; c3 : CaseA} ; 

  mkDitransVerb : 
   Verb -> Preposition -> CaseA -> Preposition -> CaseA -> DitransVerb = 
   \v,p1,c1,p2,c2 -> 
    v ** {s2 = p1 ; c = c1 ; s3 = p2 ; c3 = c2} ;

--- This must be completed to account for the order of the clitics.
--- In the rule below, the last argument cannot get cliticized.

-- The following macro builds the "ne - pas" or "non" negation. The second
-- string argument is used for the complement of a verb phrase. In Italian,
-- one string argument would actually be enough.

  posNeg : Bool -> (verb, compl : Str) -> Str ;

  DitransVerbVerb = TransVerb ** {c3 : CaseA} ;


--2 Adverbs
--
-- Adverbs are not inflected (we ignore comparison, and treat
-- compared adverbs as separate expressions; this could be done another way).
--
-- (We should also take into account clitic ones, like "y",
-- as well as the position: "est toujours heureux" / "est heureux à Paris".) 

  Adverb : Type = SS ;

  advVerbPhrase : VerbPhrase -> Adverb -> VerbPhrase = \chante, bien ->
    {s = \\v,g,n,p => chante.s ! v ! g ! n ! p ++ bien.s} ;

-- Adverbs are typically generated by prefixing prepositions.
-- The rule for prepositional phrases also comprises the use of prepositions
-- treated as cases. Therefore, both a preposition and a case are needed
-- as arguments.

  prepNounPhrase : {s : Preposition ; c : CaseA} -> NounPhrase -> Adverb = \dans,jean -> 
    {s = dans.s ++ jean.s ! Ton dans.c} ;

  justPrep : Preposition -> {s : Preposition ; c : CaseA} = \sans ->
    {s = sans ; c = prepositional} ;

  justCase : CaseA -> {s : Preposition ; c : CaseA} = \nom ->
    {s = [] ; c = nom} ;

-- This is a source of the "homme avec un téléscope" ambiguity, and may produce
-- strange things, like "les voitures toujours".
-- Semantics will have to make finer distinctions among adverbials.
-- French moreover says "les voitures d'hier" rather than "les voitures hier".

  advCommNounPhrase : CommNounPhrase -> Adverb -> CommNounPhrase = \mec,aparis ->
    {s = \\n => mec.s ! n ++ aparis.s ;
     g = mec.g
    } ;

  advAdjPhrase : Adverb -> AdjPhrase -> AdjPhrase = \trop,lent ->
    {s = \\a => trop.s ++ lent.s ! a ;
     p = lent.p
    } ;

--2 Sentences
--
-- Sentences depend on a *mode parameter* selecting between 
-- indicative and subjunctive forms.

  Sentence : Type = SS1 Mode ;

-- This is the traditional $S -> NP VP$ rule. It takes care of both
-- mode and agreement.

param
  Tense = Present | Past | Future | Condit ;

  ClForm = 
     ClPres    Anteriority Mode
   | ClImperf  Anteriority Mode
   | ClPasse   Anteriority
   | ClFut     Anteriority
   | ClCondit  Anteriority
   | ClInfinit Anteriority      -- "naked infinitive" clauses
    ;

oper
  useClForm : Tense -> Anteriority -> Mode -> ClForm = 
    \t,a,m -> case t of {
    Present => ClPres a m ;
    Past    => ClImperf a m ; --- no passé simple
    Future  => ClFut a ;      ---- mode
    Condit  => ClCondit a
    } ;

  Clause = {s : Bool => ClForm => Str} ;


--3 Sentence-complement verbs
--
-- Sentence-complement verbs take sentences as complements.
-- The mode of the complement depends on the verb, and can be different
-- for positive and negative uses of the verb 
-- ("je crois qu'elle vient" -"je ne crois pas qu'elle vienne"),

  SentenceVerb : Type = Verb ** {mp, mn : Mode} ;

  subordMode : SentenceVerb -> Bool -> Mode = \verb,b -> 
   if_then_else Mode b verb.mp verb.mn ;
  verbSent : Verb -> Mode -> Mode -> SentenceVerb = \v,mp,mn ->
    v ** {mp = mp ; mn = mn} ;

-- The embedding conjunction is language dependent.

  embedConj : Str ;


--3 Verb-complement verbs
--
-- Verb-complement verbs take verb phrases as complements.
-- They can need an oblique case ("à", "de"), but they work like ordinary verbs.

  VerbVerb : Type = Verb ** {c : CaseA} ;

  mkVerbVerbDir : Verb -> VerbVerb = \v -> v ** {c = accusative} ;

--2 Sentences missing noun phrases
--
-- This is one instance of Gazdar's *slash categories*, corresponding to his
-- $S/NP$.
-- We cannot have - nor would we want to have - a productive slash-category former.
-- Perhaps a handful more will be needed.
--
-- Notice that the slash category has the same relation to sentences as
-- transitive verbs have to verbs: it's like a *sentence taking a complement*.

  ClauseSlashNounPhrase = Clause ** Complement ;

  dummyNounPhrase : NounPhrase = mkNameNounPhrase [] Masc ;

  slashTransVerb : NounPhrase -> TransVerb -> ClauseSlashNounPhrase = \np,v -> 
    sats2clause (mkSatsObject np v dummyNounPhrase) ** 
    complementOfTransVerb v ;


  slashVerbVerb : NounPhrase -> VerbVerb -> TransVerb -> ClauseSlashNounPhrase = 
    \subj, verb, tv -> 
   sats2clause (
     insertExtrapos 
       (mkSats subj verb) 
       (\\_ =>  prepCase verb.c ++ tv.s ! VInfin)
     ) ** complementOfTransVerb tv ;

  slashAdverb : Clause -> {s : Str ; c : CaseA} -> ClauseSlashNounPhrase = 
    \ilhabite,dans -> ilhabite ** {s2 = dans.s ; c = dans.c} ;


--2 Relative pronouns and relative clauses
--
-- Relative pronouns are inflected in
-- gender, number, and case. They can also have an inherent case,
-- but this case is 'variable' in the sense that it
-- is sometimes just mediated from the correlate
-- ("homme qui est bon"), sometimes inherent to the
-- pronominal phrase itself ("homme dont la mère est bonne").

oper

  RelPron   : Type = {s : RelFormA => Str ; g : RelGen} ;

----  RelClause   : Type = {s : Bool => ClForm => Gender => Number => Person => Str} ;
  RelClause   : Type = {
    s1 : Gender => Number => Person => Str ;
    s2 : Bool => ClForm => Gender => Number => Person => Str ;
    s3 : Bool => Str
    } ;

  RelSentence : Type = {s :         Mode   => Gender => Number => Person => Str} ;

  mkGenRel : RelGen -> Gender -> Gender = \rg,g -> case rg of {
    PGen gen => gen ;
    _      => g
    } ;

-- Simple relative pronouns ("qui", "dont", "par laquelle")
-- have no inherent gender.

  identRelPron : RelPron ;

  composRelPron : Gender -> Number -> CaseA -> Str ;

-- Complex relative pronouns ("dont la mère") do have an inherent gender.

  funRelPron : Function -> RelPron -> RelPron ;

-- There are often variants, i.e. short and long forms
-- ("que" - "lequel", "dont" -"duquel"), etc.

  allRelForms : RelPron -> Gender -> Number -> CaseA -> Str ;

-- Relative clauses can be formed from both verb phrases ("qui dort") and
-- slash expressions ("que je vois", "dont je parle"). 

  relSlash : RelPron -> ClauseSlashNounPhrase -> RelClause = \dont,jeparle ->
    {s1 = \\g,n,p      => jeparle.s2 ++ allRelForms dont g n jeparle.c ; 
     s2 = \\b,cl,g,n,p => jeparle.s ! b ! cl ;
     s3 = \\_ => [] ---- should be parts of jeparle
    } ;

-- A 'degenerate' relative clause is the one often used in mathematics, e.g.
-- "nombre x tel que x soit pair".

  relSuch : Clause -> RelClause = \A ->
    {s1 = \\g,n,p => suchPron g n ; 
     s2 = \\b,cl,g,n,p => embedConj ++ A.s ! b ! cl ;
     s3 = \\_ => [] ---- should be parts of A
    } ;

  suchPron : Gender -> Number -> Str ;

-- The main use of relative clauses is to modify common nouns.
-- The result is a common noun, out of which noun phrases can be formed
-- by determiners. A comma is used before the relative clause.
--
-- N.B. subjunctive relative clauses 
-- ("je cherche un mec qui sache chanter") must have another structure
-- (unless common noun phrases are given a mode parameter...).

  modRelClause : CommNounPhrase -> RelSentence -> CommNounPhrase = \mec,quidort ->
    {s = \\n => mec.s ! n ++ quidort.s ! Ind ! mec.g ! n ! P3 ;
     g = mec.g
    } ;

--2 Interrogative pronouns
--
-- If relative pronouns are adjective-like, interrogative pronouns are
-- noun-phrase-like. We use a simplified type, since we don't need the possessive
-- forms.
--
-- N.B. "est-ce que", etc, will be added below 
-- when pronouns are used in direct questions.

  IntPron : Type = {s : CaseA => Str ; g : Gender ; n : Number} ; 

-- In analogy with relative pronouns, we have a rule for applying a function
-- to a relative pronoun to create a new one. 

  funIntPron : Function -> IntPron -> IntPron = \mere,qui -> 
    {s = \\c => 
           artDef mere.g qui.n c ++ mere.s ! qui.n ++ mere.s2 ++ qui.s ! mere.c ;
     g = mere.g ;
     n = qui.n
    } ;

-- There is a variety of simple interrogative pronouns:
-- "quelle maison", "qui", "quoi". Their definitions are language-dependent.

  nounIntPron : Number -> CommNounPhrase -> IntPron ;
  intPronWho  : Number -> IntPron ;
  intPronWhat : Number -> IntPron ;

--2 Utterances

-- By utterances we mean whole phrases, such as 
-- 'can be used as moves in a language game': indicatives, questions, imperative,
-- and one-word utterances. The rules are far from complete.
--
-- N.B. we have not included rules for texts, which we find we cannot say much
-- about on this level. In semantically rich GF grammars, texts, dialogues, etc, 
-- will of course play an important role as categories not reducible to utterances.
-- An example is proof texts, whose semantics show a dependence between premises
-- and conclusions. Another example is intersentential anaphora.

  Utterance = SS ;
  
  indicUtt : Sentence -> Utterance = \x -> ss (x.s ! Ind ++ ".") ;
  interrogUtt : QuestionSent -> Utterance = \x -> ss (x.s ! DirQ ++ "?") ;

--2 Questions
--
-- Questions are either direct ("qui a pris la voiture") or indirect 
-- ("ce qui a pris la voiture").

param 
  QuestForm = DirQ | IndirQ ;

oper
  Question     = {s : Bool => ClForm => QuestForm => Str} ;
  QuestionSent = {s :                   QuestForm => Str} ;


--3 Yes-no questions 
--
-- Yes-no questions are used both independently ("Tu es fatigué?")
-- and after interrogative adverbials ("Pourquoi tu es fatigué?").
-- It is economical to handle with these two cases by the one
-- rule, $questVerbPhrase'$. The only difference is if "si" appears
-- in the indirect form.
--
-- N.B. the inversion variant ("Es-tu fatigué?") is missing, mainly because our
-- verb morphology does not support the intervening "t" ("Marche-t-il?").
-- The leading "est-ce que" is recognized as a variant, and requires
-- direct word order.

  questClause : Clause -> Question = \cl -> 
    {s = \\b,c => table {
       DirQ   => cl.s ! b ! c ;
       IndirQ => cl.s ! b ! c
       }
    } ;

-----  questVerbPhrase : NounPhrase -> VerbPhrase -> Question ;

--3 Wh-questions
--
-- Wh-questions are of two kinds: ones that are like $NP - VP$ sentences,
-- others that are line $S/NP - NP$ sentences.
--
-- N.B. inversion variants and "est-ce que" are treated as above.

  intSlash : IntPron -> ClauseSlashNounPhrase -> Question ;

  intNounPhrase : IntPron -> NounPhrase = \ip ->
    {s = \\nf => ip.s ! pform2case nf ; g = PGen ip.g ; n = ip.n ; p = P3 ; c = Clit0} ;

--3 Interrogative adverbials
--
-- These adverbials will be defined in the lexicon: they include
-- "quand", "où", "comment", "pourquoi", etc, which are all invariant one-word
-- expressions. In addition, they can be formed by adding prepositions
-- to interrogative pronouns, in the same way as adverbials are formed
-- from noun phrases. 
--
-- N.B. inversion variants and "est-ce que" are treated as above.

  IntAdverb = SS ;

  questAdverbial : IntAdverb -> Clause -> Question = 
    \quand, jeanDort ->
    {s = \\b,cl => 
      let 
        jeandort = jeanDort.s ! b ! cl
      in
      table {
        DirQ   => quand.s ++ jeandort ; ---- est-ce que 
        IndirQ => quand.s ++ jeandort
      }
    } ;



--2 Imperatives
--
-- We only consider second-person imperatives. 
-- 
-- N.B. following the API, we don't distinguish between 
-- singular and plural "vous", nor between masculine and feminine.
-- when forming utterances.
--
-- TODO: clitics, Italian negated imperative.

  Imperative = {s : Gender => Number => Str} ;

  imperVerbPhrase : Bool -> VerbClause -> Imperative = \b,dormir -> 
    {s = \\g,n => dormir.s ! b ! Simul ! VIImperat b n ! g ! n ! P2
    } ;

  imperUtterance : Number -> Imperative -> Utterance = \n,I ->
    ss (I.s ! Masc ! n ++ "!") ;

--2 Sentence adverbials
--
-- Sentence adverbs is the largest class and open for
-- e.g. prepositional phrases.

  advClause : Clause -> Adverb -> Clause = \yousing,well ->
   {s = \\b,c => yousing.s ! b ! c ++ well.s} ;

-- Another class covers adverbials such as "autrement", "donc", which are prefixed
-- to a sentence to form a phrase.

  advSentence : SS -> Sentence -> Utterance = \donc,ildort ->
    ss (donc.s ++ ildort.s ! Ind ++ ".") ;


--2 Coordination
--
-- Coordination is to some extent orthogonal to the rest of syntax, and
-- has been treated in a generic way in the module $CO$ in the file
-- $coordination.gf$. The overall structure is independent of category,
-- but there can be differences in parameter dependencies.
--
--3 Conjunctions
--
-- Coordinated phrases are built by using conjunctions, which are either
-- simple ("et", "ou") or distributed ("et - et", "pu - ou").

  Conjunction = CO.Conjunction ** {n : Number} ;
  ConjunctionDistr = CO.ConjunctionDistr ** {n : Number} ;

--3 Coordinating sentences
--
-- We need a category of lists of sentences. It is a discontinuous
-- category, the parts corresponding to 'init' and 'last' segments
-- (rather than 'head' and 'tail', because we have to keep track of the slot between
-- the last two elements of the list). A list has at least two elements.
--
-- N.B. we don't have repetion of "que" in subordinate coordinated sentences.

  ListSentence : Type = {s1,s2 : Mode => Str} ; 

  twoSentence : (_,_ : Sentence) -> ListSentence = 
    CO.twoTable Mode ;

  consSentence : ListSentence -> Sentence -> ListSentence = 
    CO.consTable Mode CO.comma ;

-- To coordinate a list of sentences by a simple conjunction, we place
-- it between the last two elements; commas are put in the other slots,
-- e.g. "Pierre fume, Jean boit et les autres regardsnt".

  conjunctSentence : Conjunction -> ListSentence -> Sentence = 
    CO.conjunctTable Mode ;

-- To coordinate a list of sentences by a distributed conjunction, we place
-- the first part in front of the first element, the second
-- part between the last two elements, and commas in the other slots.
-- For sentences this is really not used.

  conjunctDistrSentence : ConjunctionDistr -> ListSentence -> Sentence = 
    CO.conjunctDistrTable Mode ;

--3 Coordinating adjective phrases
--
-- The structure is the same as for sentences. The result is a prefix adjective
-- if and only if all elements are prefix.

  ListAdjPhrase : Type = 
    {s1,s2 : AForm => Str ; p : Bool} ;

  twoAdjPhrase : (_,_ : AdjPhrase) -> ListAdjPhrase = \x,y ->
    CO.twoTable AForm x y ** {p = andB x.p y.p} ;

  consAdjPhrase : ListAdjPhrase -> AdjPhrase -> ListAdjPhrase =  \xs,x ->
    CO.consTable AForm CO.comma xs x ** {p = andB xs.p x.p} ;

  conjunctAdjPhrase : Conjunction -> ListAdjPhrase -> AdjPhrase = \c,xs ->
    CO.conjunctTable AForm c xs ** {p = xs.p} ;

  conjunctDistrAdjPhrase : ConjunctionDistr -> ListAdjPhrase -> AdjPhrase = \c,xs ->
    CO.conjunctDistrTable AForm c xs ** {p = xs.p} ;


--3 Coordinating noun phrases
--
-- The structure is the same as for sentences. The result is either always plural
-- or plural if any of the components is, depending on the conjunction.
-- The gender is masculine if any of the components is. A coordinated noun phrase
-- cannot be clitic.

  ListNounPhrase : Type = 
    {s1,s2 : CaseA => Str ; g : PronGen ; n : Number ; p : Person} ;

  twoNounPhrase : (_,_ : NounPhrase) -> ListNounPhrase = \x,y ->
    {s1 = \\c => x.s ! stressed c ; 
     s2 = \\c => y.s ! stressed (conjunctCase c)} ** 
    {n = conjNumber x.n y.n ; g = conjGender x.g y.g ; p = conjPers x.p y.p} ;

  consNounPhrase : ListNounPhrase -> NounPhrase -> ListNounPhrase =  \xs,x ->
    {s1 = \\c => xs.s1 ! c ++ CO.comma ++ xs.s2 ! conjunctCase c ; 
     s2 = \\c => x.s ! stressed (conjunctCase c)} ** 
    {n = conjNumber xs.n x.n ; g = conjGender xs.g x.g ; p =conjPers xs.p x.p} ;

-- French says "la somme de x et de y" whereas 
-- Italian says "la somma di x e y" and similarly for Spanish.

  conjunctCase : CaseA -> CaseA ;
--  conjunctCase c = nominative ; -- Spanish, Italian
--  conjunctCase c = c ; -- French

  conjunctNounPhrase : Conjunction -> ListNounPhrase -> NounPhrase = \co,xs ->
    {s = \\c => xs.s1 ! pform2case c ++ co.s ++ xs.s2 ! pform2case c} ** 
    {n = conjNumber co.n xs.n ; g = xs.g ; p = xs.p ; c = Clit0 } ;

  conjunctDistrNounPhrase : ConjunctionDistr -> ListNounPhrase -> NounPhrase = 
    \co,xs ->
    {s = \\c => co.s1++ xs.s1 ! pform2case c ++ co.s2 ++ xs.s2 ! pform2case c} ** 
    {n = conjNumber co.n xs.n ; g = xs.g ; p = xs.p ; c = Clit0} ;


-- We have to define a calculus of numbers of genders. For numbers,
-- it is like the conjunction with $Pl$ corresponding to $False$. For genders,
-- $Masc$ corresponds to $False$.

  conjNumber : Number -> Number -> Number = \m,n -> case <m,n> of {
    <Sg,Sg> => Sg ;
    _ => Pl 
    } ;

  conjGen : Gender -> Gender -> Gender = \m,n -> case <m,n> of {
    <Fem,Fem> => Fem ;
    _ => Masc 
    } ;

  conjGender : PronGen -> PronGen -> PronGen = \m,n -> case <m,n> of {
    <PGen Fem, PGen Fem> => PGen Fem ;
    _ => PNoGen
    } ;

-- For persons, we go in the descending order:
-- "moi et toi sommes forts", "lui ou toi es fort".
-- This is not always quite clear.

  conjPers : Person -> Person -> Person = \p,q -> case <p,q> of {
    <P3,P3> => P3 ;
    <P1,_>  => P1 ;
    <_,P1>  => P1 ;
    _       => P2
    } ;

--3 Coordinating adverbs
--
-- We need a category of lists of adverbs. It is a discontinuous
-- category, the parts corresponding to 'init' and 'last' segments
-- (rather than 'head' and 'tail', because we have to keep track of the slot between
-- the last two elements of the list). A list has at least two elements.

  ListAdverb : Type = SD2 ;

  twoAdverb : (_,_ : Adverb) -> ListAdverb = CO.twoSS ;

  consAdverb : ListAdverb -> Adverb -> ListAdverb =
    CO.consSS CO.comma ;

-- To coordinate a list of adverbs by a simple conjunction, we place
-- it between the last two elements; commas are put in the other slots,

  conjunctAdverb : Conjunction -> ListAdverb -> Adverb = \c,xs ->
    ss (CO.conjunctX c xs) ;

-- To coordinate a list of adverbs by a distributed conjunction, we place
-- the first part (e.g. "either") in front of the first element, the second
-- part ("or") between the last two elements, and commas in the other slots.

  conjunctDistrAdverb : ConjunctionDistr -> ListAdverb -> Adverb =
    \c,xs ->
    ss (CO.conjunctDistrX c xs) ;




--2 Subjunction
--
-- Subjunctions ("si", "quand", etc) 
-- are a different way to combine sentences than conjunctions.
-- The main clause can be a sentences, an imperatives, or a question,
-- but the subjoined clause must be a sentence. The inherent mood can be
-- indicative ("si", "quand") or subjunctive ("bien que").

  Subjunction = {s : Str ; m : Mode} ;

  subjunctSentence : Subjunction -> Sentence -> Sentence -> Sentence = \si,A,B ->
    {s = \\m => subjunctVariants si A (B.s ! m)
     } ;

  subjunctImperative : Subjunction -> Sentence -> Imperative -> Imperative = 
    \si,A,B -> 
    {s = \\g,n => subjunctVariants si A (B.s ! g ! n)
    } ;

  subjunctQuestion : Subjunction -> Sentence -> QuestionSent -> QuestionSent = \si,A,B ->
    {s = \\q => subjunctVariants si A (B.s ! q)
    } ;

-- There are uniformly two variant word orders, e.g. 
-- "si tu fumes je m'en vais"
-- and "je m'en vais si tu fumes".

  subjunctVariants : Subjunction -> Sentence -> Str -> Str = \si,A,B ->
    let {As = A.s ! si.m} in 
    variants {
      si.s ++ As ++ B ; 
      B ++ si.s ++ As
      } ;

--2 One-word utterances
-- 
-- An utterance can consist of one phrase of almost any category, 
-- the limiting case being one-word utterances. These
-- utterances are often (but not always) in what can be called the
-- default form of a category, e.g. the nominative.
-- This list is far from exhaustive.

  useNounPhrase : NounPhrase -> Utterance = \jean ->
    postfixSS "." (defaultNounPhrase jean) ;
  useCommonNounPhrase : Number -> CommNounPhrase -> Utterance = \n,mec -> 
    useNounPhrase (indefNounPhrase n mec) ;

  verbUtterance : VerbPhrase -> Utterance = \vp ->
    ss (vp.s ! VIInfinit ! Masc !  Sg ! P3) ; 


-- one-form variants

  defaultNounPhrase : NounPhrase -> SS = \jean -> 
    ss (jean.s ! stressed nominative) ;

  defaultQuestion : QuestionSent -> SS = \quiesttu ->
    ss (quiesttu.s ! DirQ) ;

  defaultSentence : Sentence -> SS = \x -> ss (x.s ! Ind) ;

----- moved from Types

  artDef : Gender -> Number -> CaseA -> Str ;
  artIndef : Gender -> Number -> CaseA -> Str ;
  genForms : Str -> Str -> Gender => Str ;

----- moved from Res ; should not be here (pronouns are also in MorphoIta...)

  pronJe, pronTu, pronIl, pronElle, pronNous, pronVous, pronIls, pronElles : 
    Pronoun ;
  chaqueDet, quelDet, plupartDet : Determiner ;

  commentAdv, quandAdv, ouAdv, pourquoiAdv : Adverb ;

  etConj, ouConj : Conjunction ;
  etetConj, ououConj : ConjunctionDistr ;
  siSubj, quandSubj : Subjunction ;

  ouiPhr, noPhr : Utterance ;

---------------------------------------------------------------------
---- for Sats; to be moved earlier

  Sats : Type = {
      s1 : Str ;        -- je                 je
      s3 : Str ;        -- (ne) le lui        (ne)     
      s4 : VF => Str ;  -- ai                 ai
      s5 : Str ;        -- toujours (pas)     toujours (pas)
      s6 : Str ;        -- (dit) directement  (voulu) le lui dire directement
      s7 : Bool => Str; -- qu'il pleu/pleuve
      aux : VAux ;
      g,g2 : Gender ;   -- features for main verb and participle
      n,n2 : Number ;
      p : Person        -- feature of subject
      
      } ;

  verbClForm : 
    Verb -> ClForm -> Gender -> Number -> Person -> Gender -> Number -> (Str * Str) = 
    \verb,cl,g,n,p,g2,n2 -> 
      let 
        aimee = verb.s ! VPart g2 n2 ;
        auxv  = (auxVerb verb).s ;
        aime  : TMode -> Str = \t -> verb.s ! (VFin t n p) ;
        avoir : TMode -> Str = \t -> auxv ! (VFin t n p) ;
        aimer = verb.s ! VInfin ;
        avoirr = auxv ! VInfin
      in
      case cl of {
        ClPres   Simul m => <aime  (VPres m),   []> ;
        ClPres   a m     => <avoir (VPres m),   aimee> ;
        ClImperf Simul m => <aime  (VImperf m), []> ;
        ClImperf a m     => <avoir (VImperf m), aimee> ;
        ClPasse  Simul   => <aime  VPasse,      []> ;
        ClPasse  a       => <avoir VPasse,      aimee> ;
        ClFut    Simul   => <aime  VFut,        []> ;
        ClFut    a       => <avoir VFut,        aimee> ;
        ClCondit Simul   => <aime  VCondit,     []> ;
        ClCondit a       => <avoir VCondit,     aimee> ;
        ClInfinit Simul  => <aimer,             []> ;
        ClInfinit a      => <avoirr,            aimee>
        } ;

  mkSats : NounPhrase -> Verb -> Sats = \subj,verb ->
     let ifEsse : (T : Type) -> T -> T -> T = \T,e,h ->
       case verb.aux of {
         AEsse   => e ;
         AHabere => h
         }
     in
     {s1 = subj.s ! unstressed nominative ;
      s3 = [] ;
      s4 = verb.s ;
      s5, s6 = [] ;
      s7 = \\_ => [] ;
      aux = verb.aux ;
      g = pgen2gen subj.g ;
      n = subj.n ;
      p = subj.p ;
      g2 = ifEsse Gender (pgen2gen subj.g) Masc ;
      n2 = ifEsse Number subj.n            Sg
      } ;

  insertObject : Sats -> CaseA -> Str -> NounPhrase -> Sats = \sats, c, prep, obj -> 
     let
       ifClit : (T : Type) -> T -> T -> T = 
         \T -> if_then_else T (andB (isNounPhraseClit obj) (isClitCase c)) ;
       object = obj.s ! (case2pformClit c) ;
       clit   = ifClit Str object [] ;
       np     = ifClit Str [] object
     in
     {s1  = sats.s1 ;
      s3  = sats.s3 ++ clit ; ---- or clit ++ s3, dep. on clits
      s4  = sats.s4 ;
      s5  = sats.s5 ;
      s6  = sats.s6 ++ prep ++ np ;
      s7  = sats.s7 ;
      aux = sats.aux ;
      g   = sats.g ;
      n   = sats.n ;
      g2  = ifClit Gender (pgen2gen obj.g) sats.g2 ; ---- only for clit acc
      n2  = ifClit Number obj.n sats.n2 ;
      p   = sats.p
      } ;

  insertExtrapos : Sats -> (Bool => Str) -> Sats = \sats,obj -> 
     {s1  = sats.s1 ;
      s3  = sats.s3 ;
      s4  = sats.s4 ;
      s5  = sats.s5 ;
      s6  = sats.s6 ;
      s7  = obj ;
      aux = sats.aux ;
      g   = sats.g ;
      n   = sats.n ;
      g2  = sats.g2 ;
      n2  = sats.n2 ;
      p   = sats.p
      } ;

  mkSatsObject : NounPhrase -> TransVerb -> NounPhrase -> Sats = \subj,verb,obj ->
    insertObject (mkSats subj verb) verb.c verb.s2 obj ;

  mkSatsCopula : NounPhrase -> Str -> Sats = \subj,obj ->
    mkSatsObject subj 
      (mkTransVerbDir copula)                         --- hack to reuse  
      (nameNounPhrase (mkProperName obj Masc)) ;      --- this function

  sats2clause : Sats -> Clause = 
    \sats -> {s = \\b,cf =>
      let
        je   = sats.s1 ;
        lui  = sats.s3 ;
        dire = verbClForm {s = sats.s4 ; aux = sats.aux}
                 cf sats.g sats.n sats.p sats.g2 sats.n2 ;
        ai   = dire.p1 ;
        dit  = dire.p2 ;
        toujours = sats.s5 ;
        directement = sats.s6 ;
        ne  = if_then_Str b [] negNe ;
        pas = if_then_Str b [] negPas ;
        oui = sats.s7 ! b
      in 
      je ++ ne ++ lui ++ ai ++ toujours ++ pas ++ dit ++ directement ++ oui
    } ;

  negNe, negPas : Str ;

  sats2quest : Sats -> Question = \x ->
    let cl = sats2clause x
    in
    {s = \\b,f,_ => cl.s ! b ! f} ;

  sats2rel : (Gender -> Number -> Person -> Sats) -> RelClause = \s ->
    {s1 = \\g,n,p => 
       let
         sats = s g n p ;
       in 
       sats.s1 ;
     s2 = \\b,cf,g,n,p => 
      let
        sats = s g n p ;
     
        lui  = sats.s3 ;
        dire = verbClForm {s = sats.s4 ; aux = sats.aux}
                 cf sats.g sats.n sats.p sats.g2 sats.n2 ;
        ai   = dire.p1 ;
        toujours = sats.s5 ;
        dit  = dire.p2 ;
        ne  = if_then_Str b [] negNe ;
        pas = if_then_Str b [] negPas ;
      in
      ne ++ lui ++ ai ++ toujours ++ pas ++ dit ;
     s3 = \\b =>
      let
        sats = s Masc Sg P3 ;
        directement = sats.s6 ;
        oui = sats.s7 ! b
      in 
      directement ++ oui
    } ;

{-
  sats2rel : (Gender -> Number -> Person -> Sats) -> RelClause = \sats ->
    {s = \\b,f,g,n,p => 
         (sats2clause (sats g n p)).s ! b ! f
    } ;
-}
  relNounPhrase : RelPron -> Gender -> Number -> Person -> NounPhrase = 
   \r,g,n,p -> {
    s = \\np => r.s ! npRelForm np ;
    g = PGen g ; --- r.g
    n = n ; 
    p = p ;
    c = Clit0 ;
    } ;

  sats2verbPhrase : Sats -> VerbClause = 
    \sats -> {s = \\b,ant,vi,g,n,p =>
      let
        lui  = sats.s3 ;
        dire = verbVIForm {s = sats.s4 ; aux = sats.aux} ant
                 vi g n p sats.g2 sats.n2 ;
        ai   = dire.p1 ;
        dit  = dire.p2 ;
        toujours = sats.s5 ;
        directement = sats.s6 ;
        ne  = if_then_Str b [] negNe ;
        pas = if_then_Str b [] negPas ;
        oui = sats.s7 ! b
      in 
      ne ++ lui ++ ai ++ toujours ++ pas ++ dit ++ directement ++ oui
    } ;

---- What happens to polarity and anteriority ?

  verbVIForm : 
    Verb -> Anteriority -> 
    VIForm -> Gender -> Number -> Person -> Gender -> Number -> (Str * Str) = 
    \verb,ant,cl,g,n,p,g2,n2 -> 
      let 
        aime  : Number -> Str = \t -> verb.s ! vImper t P2 ;
        aimee = case ant of {Simul => [] ; _ => verb.s ! VPart Masc Sg} ; ---- g n
        finverb = case ant of {Simul => verb.s ; _ => (auxVerb verb).s} ;
        aimer = finverb ! VInfin ;
        aimant = finverb ! VGer
      in
      case cl of {
        VIImperat _ n => <aime n, []> ;        -- no imperative perfect
        VIGerund      => <aimant, aimee> ;
        VIInfinit     => <aimer,  aimee>
        } ;

predVerb0 : Verb ->  Clause = \rain ->
  sats2clause (mkSats (pronNounPhrase pronImpers) rain) ;

progressiveSats : NounPhrase -> VerbPhrase -> Sats ;

}
