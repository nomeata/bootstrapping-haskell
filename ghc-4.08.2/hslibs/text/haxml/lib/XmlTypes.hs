module XmlTypes where

{-
   This module defines an internal (generic) representation for XML
   documents including their DTDs.

   The original module Types was derived by hand from the XML specification,
   following the grammar precisely.  Module STypes was a simplification of
   Types, removing layers of indirection and redundancy, and generally
   making things easier to work with.  Module PETypes went a
   stage further in allowing PEReferences to be ubiquitous, by removing
   them from the types and resolving all PE references at parse-time.
   And now, in module XmlTypes, we add in a per-document symbol table
   for GEReferences, and a whitespace-significance flag for plaintext.
-}

#if !defined NO_FINITE_MAP
import FiniteMap
#endif

data Document = Document Prolog (SymTab EntityDef) Element
data Prolog   = Prolog (Maybe XMLDecl) (Maybe DocTypeDecl)
data XMLDecl  = XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl) 
data Misc     = Comment Comment
              | PI ProcessingInstruction
              
type ProcessingInstruction = (PITarget,String)

type SDDecl      = Bool 
type VersionInfo = String 
type Comment     = String 
type PITarget    = String 

data DocTypeDecl = DTD Name (Maybe ExternalID) [MarkupDecl] 
data MarkupDecl  = Element  ElementDecl
                 | AttList  AttListDecl
                 | Entity   EntityDecl
                 | Notation NotationDecl
                 | MarkupMisc Misc

data ExtSubset     = ExtSubset (Maybe TextDecl) [ExtSubsetDecl] 
data ExtSubsetDecl = ExtMarkupDecl MarkupDecl
                   | ExtConditionalSect ConditionalSect

data Element   = Elem Name [Attribute] [Content]
data ElemTag   = ElemTag Name [Attribute]	-- intermediate for parsing
type Attribute = (Name, AttValue)
data Content   = CElem Element
               | CString Bool CharData	-- bool is whether whitespace is significant
               | CRef Reference
               | CMisc Misc

data ElementDecl = ElementDecl Name ContentSpec
data ContentSpec = EMPTY
                 | ANY
                 | Mixed Mixed
                 | ContentSpec CP
data CP = TagName Name Modifier
        | Choice [CP] Modifier
        | Seq [CP] Modifier 
data Modifier = None  --JustOne
              | Query --ZeroOrOne
              | Star  --ZeroOrMore
              | Plus  --OneOrMore 
data Mixed = PCDATA
           | PCDATAplus [Name] 

data AttListDecl = AttListDecl Name [AttDef]
data AttDef      = AttDef Name AttType DefaultDecl 
data AttType     = StringType
                 | TokenizedType TokenizedType
                 | EnumeratedType EnumeratedType 
data TokenizedType = ID
                   | IDREF
                   | IDREFS
                   | ENTITY
                   | ENTITIES
                   | NMTOKEN
                   | NMTOKENS 
data EnumeratedType = NotationType NotationType
                    | Enumeration Enumeration 
type NotationType   = [Name]	-- nonempty list
type Enumeration    = [NmToken]	-- nonempty list
data DefaultDecl    = REQUIRED
                    | IMPLIED
                    | Default AttValue (Maybe FIXED) 
data FIXED          = FIXED 

data ConditionalSect = IncludeSect IncludeSect
                     | IgnoreSect IgnoreSect 
type IncludeSect = ExtSubsetDecl
type IgnoreSect  = [IgnoreSectContents]
newtype Ignore   = Ignore String 
data IgnoreSectContents = IgnoreSectContents Ignore [(IgnoreSectContents,Ignore)] 

data Reference    = RefEntity EntityRef
                  | RefChar CharRef 
                  deriving Eq
type EntityRef    = Name 
type CharRef      = String 
type PEReference  = Name 

data EntityDecl   = EntityGEDecl GEDecl
                  | EntityPEDecl PEDecl 
data GEDecl       = GEDecl Name EntityDef 
data PEDecl       = PEDecl Name PEDef 
data EntityDef    = DefEntityValue EntityValue
                  | DefExternalID ExternalID (Maybe NDataDecl) 
data PEDef        = PEDefEntityValue EntityValue
                  | PEDefExternalID ExternalID 
data ExternalID   = SYSTEM SystemLiteral
                  | PUBLIC PubidLiteral SystemLiteral 
newtype NDataDecl = NDATA Name  

data TextDecl     = TextDecl (Maybe VersionInfo) EncodingDecl 
data ExtParsedEnt = ExtParsedEnt (Maybe TextDecl) Content 
data ExtPE        = ExtPE (Maybe TextDecl) ExtSubsetDecl 

data NotationDecl    = NOTATION Name (Either ExternalID PublicID) 
newtype PublicID     = PUBLICID PubidLiteral 
newtype EncodingDecl = EncodingDecl String 

type Name     = String		 -- non-empty string
type Names    = [Name]		 -- non-empty list
type NmToken  = String		 -- non-empty string
type NmTokens = [NmToken]	 -- non-empty list

data AttValue    = AttValue [Either String Reference]
                 deriving Eq
data EntityValue = EntityValue [EV] 
data EV = EVString String
     -- | EVPERef PEReference
        | EVRef Reference 
newtype PubidLiteral  = PubidLiteral String 
newtype SystemLiteral = SystemLiteral String 
type CharData         = String 
type CDSect           = CharData

instance Eq ElemTag where
    (ElemTag n _) == (ElemTag m _)  = n==m

---- Symbol table stuff ----

emptyST  :: SymTab a
addST    :: String -> a -> SymTab a -> SymTab a
lookupST :: String -> SymTab a -> Maybe a

#if defined(NO_FINITE_MAP)
type SymTab a = [(String,a)]
emptyST     = emptyFM
addST n v   = ((n,v):)
lookupST    = lookup
#else
type SymTab a = FiniteMap String a
emptyST     = emptyFM
addST n v s = addToFM s n v
lookupST    = flip lookupFM
#endif
