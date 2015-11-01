#ifndef REGULAR_EXPRESSION_H_DANIEL
#define REGULAR_EXPRESSION_H_DANIEL
/* 
* Coded By    : Daniel Herman Shmulyan
* Description : Proccesing regular expressions. Given an expression and a string returns the longest substring matching the expression. 
*				See examples in the implementation of regex_debug() function
*				Based on rules described at: http://en.wikipedia.org/wiki/Regular_expression.
*				This is a recursive regex (It does not compile regular expression to automaton like described in http://swtch.com/~rsc/regexp/regexp1.html)
*               But it uses uses constant small amount of memory for compilation and is extremely fast
*				Designed to work on simple regular expressions (ASCII only and no longer than 256 bytes)
*/

/****************************************************************************/
// Regular expression definitions and rules:
//		Single character is matched like 'abc' to 'zabcd' from place 1, length 3.
//		. - is matched to any character. 'a..d' matches 'abcd', 'aaad','aqwd'
//		* - 0 or more occurences: 'a*' is matched to '', 'a', 'aa', 'aaaaaa'
//		+ - 1 or more occurences: 'a+' is matched to   , 'a', 'aa', 'aaaaaa' but not ''
//		? - 0 or 1    occurences: 'ab?c' is matched to 'ac' and 'abc'
//		{N} - exactly N occurences: 'a{4}' is matched to 'aaaa'
//		{M,N} - Between M to N occurences: 'a{1,3}' is matched to 'a','aa','aaa' but not '' nor 'aaaaa'
//		{M,}  - At least M but with no maximum limit. 'a{2,} is matched to 'aa','aaaaa','aaaaaaaaa'
//		Use \\ to inclde reserved characters '\\[p\\]*' is matched to '[p]'
//		[]  - Matches a single character that is contained within the brackets. [abc] is matched to 'a', 'b', 'c' but not 'd'
//		[^] - Invert selection: [^abc] is matched to any possible character except 'a', 'b', 'c'
//		[a-z] - Selection range: [A-Z] is matched to any capital english letter
//		[0-9a-zA-Z\\-\\+\\.] is matched to any alpha-numeric character (part of number or english word)
//		() - Used to group expression. Like:  '(ab)*' is mathced to 'abababab'
//		|  - Choose one of alternative expressions: (cat|dog|au+a) is matched 'cat' or 'dog' or 'aua' or 'auua' or 'auuua' etc
//		You can use any combinations of the above, Example: (_[hc]?|me) matches "_hat", "_cat", "_at and "me".
//
// Known Issues:
//		Since we use recursion on (), the greedy '.*' mechanism may cause '*' to grab too much characters. Example: "(.*A)BA" in "ABA" will return NULL since '.*' is wrongly matched to 'AB'.
//		No multi threading support (a single regex matching cannot run of a few CPU's and nither can a few regex matches run in parallel).
// Special first character (appears as first character of the regular expression  pattern).
//		^ - Match to beggining of the string. Example: "ab" matches "zab" at place 1 but "^ab" does not match "zab" at all.
//		$ - Match to the end. Not supported yet. Daniel, Todo...  Example: 'ab' matches 'abz' with length 2, but '$ab' will not match 'abz' at all.
//		] - Chose last match. Not supported yet. Daniel, Todo...  Example: 'ab' will match 'ab1ab3' at first 2 characters. but ']ab' will match to the second (last) appearance of 'ab'

// Abbreviations:
//		\d       ie. [0-9]          digit
//		\D       ie. [^0-9]         non-digit
//		\x       ie. [0-9A-Fa-f]    hex digit
//		\X       ie. [^0-9A-Fa-f]
//		\w       ie. [0-9A-Za-z_]   word character         
//		\W       ie. [^0-9A-Za-z_]
//		\h       ie. [0-9A-Za-z]    head of word character     
//		\H       ie. [^0-9A-Za-z]
//		\a       ie. [A-Za-z]       alphabetic character     
//		\A       ie. [^A-Za-z]
//		\l       ie. [a-z]          lowercase character     
//		\L       ie. [^a-z]
//		\u       ie. [A-Z]          uppercase character     
//		\U       ie. [^A-Z]
//		\s		 ie. [ \t\r\n\v\f]	Whitespace characters
//		\S		 ie. [^ \t\r\n\v\f]	Non-whitespace characters

/****************************************************************************/
/******************************** Functions *********************************/
/****************************************************************************/
// Structures for compilation of regex pattern (faster execution of regex, at cost of initial calculation and memory usage). The faster execution is achieved by precalculating for each expression its length
struct tCompiledRegex{
private:
    const char* start;					// Pointer to beggining of the pattern
	const char* end;					// Pointer to the end of the pattern 
	unsigned char exprLen[ 256];		// For each sub expression - stores its length (faster jumping between expressions). Supports maximal expression length of 256.
	unsigned char unionLen[256];		// For each union set selection (aab|ccd|ef) - stores its length (faster jumping between union sets)

public:
	const char* getExpressionEnd(         const char *ex) const { return ex + exprLen[ (int)(ex-start)]; }		// Given a pointer to beggining of sub expresion, returns pointer to its end
	const char* getExpressionEnd_UnionSet(const char *ex) const { return ex + unionLen[(int)(ex-start)]; }		// Same for union set 
	
	const char* compile(const char *pattern);							// Assumes the pattern is legal expresion no longer than 256 bytes and compiles the pattern. Returns the end of the pattern on success or NULL on failure.
	const char* search( const char *sampleString, int* resLen) const;	// Returns a pointer to the beginning of the matched part and sets resLen to store the length of the mathced part.
};

const char* regex_search(const char* pattern, const char* sampleString, int* resLen); // Compile and search in one function. A C style API.

#endif // H 
/****************************************************************************/
// EOF.
