/* unlit.c                                   Wed Dec  5 17:16:24 GMT 1990
 *
 * Literate script filter.  In contrast with the format used by most
 * programming languages, a literate script is a program in which
 * comments are given the leading role, whilst program text must be
 * explicitly flagged as such by placing a `>' character in the first
 * column on each line.  It is hoped that this style of programming will
 * encourage the writing of accurate and clearly documented programs
 * in which the writer may include motivating arguments, examples
 * and explanations.  
 *
 * Unlit is a filter that can be used to strip all of the comment lines
 * out of a literate script file.  The command format for unlit is:
 *              unlit [-n] [-q] ifile ofile
 * where ifile and ofile are the names of the input (literate script) and
 * output (raw program) files respectively.  Either of these names may
 * be `-' representing the standard input or the standard output resp.
 * A number of rules are used in an attempt to guard against the most
 * common errors that are made when writing literate scripts:
 * 1) Empty script files are not permitted.  A file in which no lines
 *    begin with `>' usually indicates a file in which the programmer
 *    has forgotten about the literate script convention.
 * 2) A line containing part of program definition (i.e. preceeded by `>')
 *    cannot be used immediately before or after a comment line unless
 *    the comment line is blank.  This error usually indicates that
 *    the `>' character has been omitted from a line in a section of
 *    program spread over a number of lines.
 * Using the -q (quiet) flag suppresses the signalling of these error
 * conditions.  The default behaviour can be selected explicitly using
 * the -n (noisy) option so that any potential errors in the script file
 * are reported.
 *
 * The original idea for the use of literate scripts is due to Richard
 * Bird of the programming Research Group, Oxford and was initially
 * adopted for use in the implementation of the functional programming
 * language Orwell used for teaching in Oxford.  This idea has subsequently
 * been borrowed in a number of other language implementations.
 *
 * Modified to understand \begin{code} ... \end{code} used in Glasgow.  -- LA
 * And \begin{pseudocode} ... \end{pseudocode}.  -- LA
 */

#include <stdio.h>
#include <ctype.h>

#define NULLSTR        ((char *)0)
#define DEFNCHAR       '>'
#define MISSINGBLANK   "unlit: Program line next to comment"
#define EMPTYSCRIPT    "unlit: No definitions in file (perhaps you forgot the '>'s?)"
#define USAGE          "usage: unlit [-q] [-n] [-c] file1 file2\n"
#define CANNOTOPEN     "unlit: cannot open \"%s\"\n"
#define DISTINCTNAMES  "unlit: input and output filenames must differ\n"
#define MISSINGENDCODE "unlit: missing \\end{code}\n"

#define BEGINCODE "\\begin{code}"
#define LENBEGINCODE 12
#define ENDCODE "\\end{code}"
#define LENENDCODE 10
#ifdef PSEUDOCODE
/* According to Will Partain, the inventor of pseudocode, this gone now. */
#define MISSINGENDPSEUDOCODE "unlit: missing \\end{pseudocode}\n"
#define BEGINPSEUDOCODE "\\begin{pseudocode}"
#define LENBEGINPSEUDOCODE 18
#define ENDPSEUDOCODE "\\end{pseudocode}"
#define LENENDPSEUDOCODE 16
#endif

typedef enum { START, BLANK, TEXT, DEFN, BEGIN, /*PSEUDO,*/ END, HASH, SHEBANG } line;
#define isWhitespace(c)  (c==' '  || c=='\t')
#define isLineTerm(c)    (c=='\n' || c==EOF)

static int noisy  = 1;   /* 0 => keep quiet about errors, 1 => report errors */
static int errors = 0;   /* count the number of errors reported              */
static int crunchnl = 0; /* don't print \n for removed lines                 */
static int leavecpp = 1; /* leave preprocessor lines */
static int ignore_shebang = 1; /* Leave out shebang (#!) lines */

/* complain(file,line,what)
 *
 * print error message `what' for `file' at `line'.  The error is suppressed
 * if noisy is not set.
 */

complain(file, lin, what)
char *file;
char *what;
int lin; {
    if (noisy) {
        if (file)
            fprintf(stderr, "%s ", file);
        fprintf(stderr,"line %d: %s\n",lin,what);
        errors++;
    }
}

#define TABPOS 8

/* As getc, but does TAB expansion */
int
egetc(istream)
FILE *istream;
{
    static int spleft = 0;
    static int linepos = 0;
    int c;

    if (spleft > 0) {
	spleft--;
	linepos++;
	return ' ';
    }
    c = getc(istream);
    if (c == EOF)
	return c;
    else if (c == '\n' || c == '\f') {
	linepos = 0;
	return c;
    } else if (c == '\t') {
	spleft = TABPOS - linepos % TABPOS;
	spleft--;
	linepos++;
	return ' ';
    } else {
	linepos++;
	return c;
    }

}

/* readline(istream, ostream)
 *
 * Read a line from the input stream `istream', and return a value
 * indicating whether that line was:
 *     BLANK (whitespace only),
 *     DEFN  (first character is DEFNCHAR),
 *     TEXT  (a line of text)
 *     BEGIN (a \begin{code} line)
 *     PSEUDO (a \begin{pseodocode} line)
 *     HASH  (a preprocessor line)
 * or  END   (indicating an EOF).
 * Lines of type DEFN are copied to the output stream `ostream'
 * (without the leading DEFNCHAR).  BLANK and TEXT lines are
 * replaced by empty (i.e. blank lines) in the output stream, so
 * that error messages refering to line numbers in the output file
 * can also be used to locate the corresponding line in the input
 * stream.
 */

line readline(istream,ostream)
FILE *istream, *ostream; {
    int c, c1;
    char buf[100];
    int i;

    c = egetc(istream);

    if (c==EOF)
        return END;
  
    if ( c == '#' ) {
      if ( ignore_shebang ) {
         c1 = egetc(istream);
         if ( c1 == '!' ) {
           while (c=egetc(istream), !isLineTerm(c)) ;
           return SHEBANG;
	 }
	 putc(c, ostream);
	 c=c1;
      }
      if ( leavecpp ) {
	putc(c, ostream);
        while (c=egetc(istream), !isLineTerm(c))
            putc(c,ostream);
        putc('\n',ostream);
        return HASH;
      }
    }

    if (c==DEFNCHAR) {
/*	putc(' ',ostream);*/
        while (c=egetc(istream), !isLineTerm(c))
            putc(c,ostream);
        putc('\n',ostream);
        return DEFN;
    }

    if (!crunchnl)
	putc('\n',ostream);

    while (isWhitespace(c))
        c=egetc(istream);
    if (isLineTerm(c))
        return BLANK;

    i = 0;
    buf[i++] = c;
    while (c=egetc(istream), !isLineTerm(c))
        if (i < sizeof buf - 1)
	    buf[i++] = c;
    while(i > 0 && isspace(buf[i-1]))
	i--;
    buf[i] = 0;
    if (strcmp(buf, BEGINCODE) == 0)
	return BEGIN;
#ifdef PSEUDOCODE
    else if (strcmp(buf, BEGINPSEUDOCODE) == 0)
	return PSEUDO;
#endif
    else
	return TEXT;
}


/* unlit(file,istream,ostream)
 *
 * Copy the file named `file', accessed using the input stream `istream'
 * to the output stream `ostream', removing any comments and checking
 * for bad use of literate script features:
 *  - there should be at least one BLANK line between a DEFN and TEXT
 *  - there should be at least one DEFN line in a script.
 */

unlit(file, istream, ostream)
char *file;
FILE *istream;
FILE *ostream; {
    line last, this=START;
    int  linesread=0;
    int  defnsread=0;

    do {
        last = this;
        this = readline(istream, ostream);
        linesread++;
        if (this==DEFN)
            defnsread++;
        if (last==DEFN && this==TEXT)
            complain(file, linesread-1, MISSINGBLANK);
        if (last==TEXT && this==DEFN)
            complain(file, linesread, MISSINGBLANK);
	if (this == BEGIN) {
	    /* start of code, copy to end */
	    char lineb[1000];
	    for(;;) {
		if (fgets(lineb, sizeof lineb, istream) == NULL) {
		    complain(file, linesread, MISSINGENDCODE);
		    exit(1);
		}
		linesread++;
		if (strncmp(lineb,ENDCODE,LENENDCODE) == 0) {
		    putc('\n', ostream);
		    break;
		}
		fputs(lineb, ostream);
	    }
	    defnsread++;
	}
#ifdef PSEUDOCODE
	if (this == PSEUDO) {
	    char lineb[1000];
	    for(;;) {
		if (fgets(lineb, sizeof lineb, istream) == NULL) {
		    complain(file, linesread, MISSINGENDPSEUDOCODE);
		    exit(1);
		}
		linesread++;
		putc('\n', ostream);
		if (strncmp(lineb,ENDPSEUDOCODE,LENENDPSEUDOCODE) == 0) {
		    break;
		}
	    }
	}
#endif
    } while(this!=END);

    if (defnsread==0)
        complain(file,linesread,EMPTYSCRIPT);
}

/* main(argc, argv)
 *
 * Main program.  Processes command line arguments, looking for leading:
 *  -q  quiet mode - do not complain about bad literate script files
 *  -n  noisy mpde - complain about bad literate script files.
 *  -r  remove cpp droppings in output.
 * Expects two additional arguments, a file name for the input and a file
 * name for the output file.  These two names must normally be distinct.
 * An exception is made for the special name "-" which can be used in either
 * position to specify the standard input or the standard output respectively.
 */

main(argc,argv)
int argc;
char **argv; {
    FILE *istream, *ostream;
    char *file;

    for (argc--, argv++; argc > 0; argc--, argv++)
        if (strcmp(*argv,"-n")==0)
            noisy = 1;
        else if (strcmp(*argv,"-q")==0)
            noisy = 0;
        else if (strcmp(*argv,"-c")==0)
	    crunchnl = 1;
        else if (strcmp(*argv,"-#")==0)
	    ignore_shebang = 0;
        else
            break;

    if (argc!=2) {
        fprintf(stderr, USAGE);
        exit(1);
    }

    if (strcmp(argv[0],argv[1])==0 && strcmp(argv[0],"-")!=0) {
        fprintf(stderr, DISTINCTNAMES);
        exit(1);
    }

    file = argv[0];
    if (strcmp(argv[0], "-")==0) {
        istream = stdin;
        file    = "stdin";
    }
    else
        if ((istream=fopen(argv[0], "r")) == NULL) {
            fprintf(stderr, CANNOTOPEN, argv[0]);
            exit(1);
        }

    if (strcmp(argv[1], "-")==0) 
        ostream = stdout; 
    else
        if ((ostream=fopen(argv[1], "w")) == NULL)  {
            fprintf(stderr, CANNOTOPEN, argv[1]);
            exit(1);
        }

    unlit(file, istream, ostream);

    fclose(istream);
    fclose(ostream);

    exit(errors==0 ? 0 : 1);
}
