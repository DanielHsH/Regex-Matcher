// Includes
#include "reg_exp.h"
#include <stdio.h>								// printfs
#include <string.h>								// memory operations like memcmp() memset()

/****************************************************************************/
// Basic definitions of types. Feel free to remove if you have them already defined.
#ifndef FALSE
	#define	FALSE		(0)
#endif
#ifndef TRUE
	#define	TRUE		(1)
#endif
#ifndef MAX
	#define MAX(a,b)	((a) > (b) ? (a) : (b))
#endif
typedef unsigned char		MYBOOL, UCHAR;		// Feel free to change boolean to 'int' or C++ 'bool'. 
#if defined(__ANDROID__) || defined(__APPLE__) || defined(__MACH__)    
	typedef __int32_t			INT32;			// Android and iOS specific INT32. Feel free to switch to 64bits integer
	#define strcpy_s(dst,size,src)			strcpy(dst,src)			// Forward compatibility to safe I/O methods. Activate only if needed (not needed for windows devices)
#else
	typedef signed   __int32    INT32;			// Windows
#endif

/****************************************************************************/
// Fast Comparison of 8,16,32 bits int - without branching. Use in loops where speed is crucial. 
#define		   is_int_negative(       num1     ) ((((INT32) (num1)        )&0x80000000)>>31)	// (num1<0   ) ? 1 : 0
inline INT32 is_int_notZero( INT32  num1     ){	return is_int_negative(num1|-num1); }			// (num1!=0  ) ? 1 : 0, Since (-X)|X is 0 for X==0 and negative for every other X.
#define		   is_int_notEqual(	   num1,num2)	is_int_notZero(num1^num2)						// (num1!=num2)? 1 : 0
#define		   is_int_equal(	   num1,num2)	(is_int_notZero(num1^num2)^0x1)					// (num1==num2)? 1 : 0
#define        is_int_inRange(i,L,U)  ((((INT32((i)-(L)))|(INT32((U)-(i))))^0x80000000)>>31)	// ((i>=L)&&(i<=U) ? 1 : 0
#define        isDigit(               chr)       is_int_inRange(chr,'0','9')					// ('9'>=chr>='0') ? 1 : 0

/****************************************************************************/
static int atoui_simple(const char* s){			// Read positive integer from string. Like "123" is converted to 123.
	int resI = 0;															
	for (;isDigit(*s); s++)
		resI = resI*10 + (s[0]-'0');
	return resI;
}

/****************************************************************************/
// Define the types of special regex character commands (as bits). 
#define		TYPE_CHAR				(0)			// Default 0 -  Single character. Like: A b 7 .
#define     TYPE_PREFIX				(1)			// Sepcial '\\' command for Abbreviations and using special characters. like \\?. This is the only command that actually uses 2 characters
#define     TYPE_SUFFIX				(2)			// Command has iteration suffix like a*, z+, b{3,5} 
#define     TYPE_OPEN				(4)			// Left Parentheses: On of the following { ( [ . This rule opens a sub expression
#define     TYPE_CLOSE				(8)			// Right parentheses: } ) ]
#define     TYPE_RECURSION			(16)		// Termination of recursive call. (command is a suffix of previous one. Like previous is 'A' and current is '{2}') Todo: Not used yet

#define		NO_MATCH				(-1)		// Returned when regular expression cannot be matched to the string.
/****************************************************************************/
// Define a command structure (single language rule)
typedef struct{
    char	id;			// The character which represennts the command. Like: * ? [
    char	attr;		// Type of the command. Can be combination of above types
    void*	f;			// Pointer to function which proccesses the current command. Polymorphism (C style)
} Cmd;

/****************************************************************************/
// Total there are 6 functions
// First parameter is the pattern. Second is the sample string and optional third is pointer to the end of the pattern
// Each one returns the number of consumed characters in sample string or NO_MATCH if inapplicable 
// (like 'a+' was applied on 'bbb'). Note 'a*' can be applied on 'bbb' and it consumes zero characters.
static inline int c_achar(	 const char* pat, const char* sam);					 // Handling single char comparison. Like 'b'
static inline int c_any(	 const char* pat, const char* sam);					 // Handlling 'any' comparison. '.'  Note: '.' may consume few bytes (1 char) when working with unicodes, and only 1 byte (= 1 char) for ascii.
static inline int c_extended(const char* pat, const char* sam);					 // Handaling special extended abreviations starting with '\\'
static inline int c_group(	 const char* pat, const char* sam);					 // Sub pattern. Grouping characters. Like (the) when searching inside 'I am the master of the realm'
static inline int c_option(	 const char* pat, const char* sam);					 // Selection of one option to match [aA]ce mathces both words: Ace and ace
static inline int c_multi(	 const char* pat, const char* sam, const char* endp);// Multiple occurance of the character. Like A+, A* A{4} A?

// Define pointers to functions that process the special commands. 2 Types
typedef int (*Stand_Func)(const char* pat, const char* sam                 );	// Standard functions and characters like () . [] \\ A 7 ....
typedef int (*SuffixFunc)(const char* pat, const char* sam,const char* endp);	// Suffix functions for multiple occurences like * + ? {}

/****************************************************************************/
// Define the table of command (regex rules). For each id, it's length, type of command and processing function
// Rules of commands: TYPE_CLOSE follows TYPE_OPEN immediately in command table
static const Cmd cmd_tbl[] = {
    '(', TYPE_OPEN|TYPE_RECURSION,		(void*)c_group,
    ')', TYPE_CLOSE|TYPE_RECURSION,		(void*)NULL,        
    '|', TYPE_CLOSE|TYPE_RECURSION,		(void*)NULL,        
    '[', TYPE_OPEN,						(void*)c_option,     
    ']', TYPE_CLOSE,					(void*)NULL,        
    '{', TYPE_SUFFIX|TYPE_OPEN,			(void*)c_multi,    
    '}', TYPE_CLOSE,					(void*)NULL,       
    '*', TYPE_SUFFIX,					(void*)c_multi,    
    '+', TYPE_SUFFIX,					(void*)c_multi,    
    '?', TYPE_SUFFIX,					(void*)c_multi,    
   '\\', TYPE_PREFIX,					(void*)c_extended, 
    '.', TYPE_CHAR,						(void*)c_any,         
      0, TYPE_CHAR,						(void*)c_achar,    
};

#define cmdLength(  cmd)		(1 + ((cmd)->attr&TYPE_PREFIX))		// All commands take 1 character + optional prefix character
#define    isSuffix(cmd)		(     (cmd)->attr&TYPE_SUFFIX)		// Does current command is a suffix of previous one. Like previous is 'A' and current is '{2}'
#define    isOpen(  cmd)        (     (cmd)->attr&TYPE_OPEN  )		// Does this command opens a sub expression
// Inverse table of the above (given a character like '*', 'C', '\' get the appropriate command). We use a look up table for all possible ASCII characters
static const Cmd* get_cmd_byChar[128];
static int        isInitialized = 0;								// Was the look up table above initialized
#define get_cmd(c)               get_cmd_byChar[(c)&0x7F]			// Get the command strucutre by character. For '(' will return '(' command
#define isReservedSymbol(c)      (get_cmd(c)->id != 0)				// Is the given character a reserved symbol (used as regex command)

/****************************************************************************/
/****************************** Aux functions *******************************/
/****************************************************************************/
// Initialize the get_cmd_byChar[] look up table
static inline void init_regex_mechanism_private(void){
	if (isInitialized) return;
    const Cmd* cmd = cmd_tbl, *end = cmd_tbl;
    while (end->id) end++;									// Find the last default command (character processing) and store it in 'end'
	for (int i=0; i<128; i++)
		get_cmd_byChar[i]		= end;						// Set the whole look up table to point to the default command
	for (; cmd<end; cmd++)
		get_cmd_byChar[cmd->id] = cmd;						// For all the real commands set an entry in the look up table
	isInitialized = TRUE;
}

/****************************************************************************/
// Find the end of the string.
static inline const char* endOfString(const char *str){
	while (*str) str++;
	return str;
}

/****************************************************************************/
// Find first occurence of character in the string. Returns the poitner in 'str' starting at 'c' or NULL if not found
static inline const char* findFirstCinS(const char *str, const char c){
	while ((*str)&&(*str-c)) str++;
	return (*str) ? str : NULL;
}

/****************************************************************************/
// Given an expression starting with left parentheses 'p' return a pointer after the end of this expression (right parentheses 'rp').
// Example: given p=[a*(1+3)[z@][aa]]tp5 returns pointer to: 'tp5' by skipping
//          the [a*(1+3)[z@][aa]] expression.
// Note: handles nested parentheses by remembring the parentheses depth. Example: (((X)Y)Z)A  - X is in depth 3, Z - in depth 1, A in depth zero.
// Extension _uc means uncompiled. needs to read forward in real time to find the end of the expression.
static inline const char* findExpressionEnd_uc(const char *p, const char rp){
	char	lp  = *p;															// left parethesis
	int		depth  = 1;															// amount of '('- amount of ')'.
	MYBOOL	isValidCommand = TRUE;												//  \\[ doesn't count as valid parentheses since it should be treated as part of the text.
	for (p++; is_int_notZero(*p)&is_int_notZero(depth); p++){
		depth += isValidCommand * (is_int_equal(*p,lp) - is_int_equal(*p,rp));	// Update the depth only if parenthesis is valid. each 'lp' causes +1, rp causes -1
		isValidCommand = is_int_notEqual(*p,'\\');			// If current charachter == '\' than the parentheses become invalid
	}
	return (depth==0) ? p : NULL;							// (depth==0) -> Expresion parentheses was matched, otherwise end of string reached and Parentheses are not balanced
}

/****************************************************************************/
// Same as findExpressionEnd_uc() but supports the the set-union extended POSIX defenition.
// This method considers the symbol '|' as closing parentheses. So (aab|ccd|ef) will return 'aab' as first expression.
//          On the second execution will return 'ccd' and on the third will return 'ef'.
// Extension _uc means uncompiled. needs to read forward in real time to find the end of the expression.
static inline const char* findExpressionEnd_UnionSet_uc(const char *p, const char lp, const char rp){
	int		depth  = 1;						
	MYBOOL	isValidCommand = TRUE;			
	for (p++; (*p!='\0')&&(depth!=0); p++){
		depth += isValidCommand * (is_int_equal(*p,lp) - is_int_equal(*p,rp));
		depth -= is_int_equal(depth,1)&is_int_equal(*p,'|');					// '|' affects only on top level. For example: (XXX(a|B|)DDD) the '|' are not possible alternatives
		isValidCommand = is_int_notEqual(*p,'\\');			
	}
	return (depth==0) ? p : NULL;			
}

/****************************************************************************/
// Find next unit of pattern
static const char* goToNextPat_uc(const char* cur){   	
    const Cmd* cmd = get_cmd(*cur);						// Get the command	
    if isOpen(cmd)										// If this is open command: [,(,{ than search for closeing character Otherwise just advance forward
		return findExpressionEnd_uc(cur,(cmd+1)->id);	// Find the closing parentheses of cur.	
	return cur + cmdLength(cmd);						// Just skip the command
}

/****************************************************************************/
/****************************** Compilation *********************************/
/****************************************************************************/
// Assumes the pattern is legal. Compiles it into 'C'. Returns the end of the pattern on success or NULL on failure.
const char* tCompiledRegex::compile(const char *pat){
	init_regex_mechanism_private();
	start = pat;																			// Pointer to the pattern
	// For each 'OPEN' rules calculate the length of the expression. Todo: make it O(n) instead of O(n^2) for worst case of "((((((((((A))))))))))"
	for (int i = 0; *pat; pat++, i++)
		exprLen[i]   = (UCHAR)(goToNextPat_uc(pat) - pat);									// 'i' is alwyas equals to (pat - start). Initialize the length of current 
	end = pat;
	
	// For each '(' ')' rules calculate union set if relevant. Like (A(z*)A|BB|CC)
	memset(unionLen,0,sizeof(*unionLen)*(end-start));
	pat = start;
	for (int i = 0; *pat; pat++, i++){
		if ((*pat!='|')||(pat[-1]=='\\'))													// We don't care about non unions.
			unionLen[i] = exprLen[i];
		else if (unionLen[i]==0){															// If we already calculated the length for current union, skip it.
			// We are by definition at the first union. Example: For (AA|BB|CC), We are at |BB|CC). 
			int open;
			for (open = i-1; start + open + exprLen[open] <= pat; open--);					// Go backwards until we find the '(' of the current union. 
			// OK, now start+open points exactly to the '(' that opened a union. Moreover 'pat' points to the first '|'
			const char *next = pat, *cur = start+open;										// Iterate over all the '|' and for each store the length until the next '|'
			while (*next =='|'){
				unionLen[cur-start] = (UCHAR)(next - cur + 1);								// Mark the current '|'
				cur = next;																	// Advance to the next '|' or the terminating ')'
				next = findExpressionEnd_UnionSet_uc(next, '(',')') -1;
			}
			unionLen[cur-start] = (UCHAR)(next - cur + 1);									// MArk for the last '|' the length until the terminating ')'
		} 
	}
	return end;
}

static const tCompiledRegex* compiledRegexPtr = NULL;	// Pointer to the current regex-used. This line ruins multi-threading capabilities. Feel free to change the system architecture to support mutli-threading
static const char*	 		 EOS = NULL;				// Pointer to the end of current processed sample. Same reason as above

/****************************************************************************/
/**************************** Command Handlers ******************************/
/****************************************************************************/
// Main method. Matches pattern to sample string. Returns the number of used characters
// Or NO_MATCH if impossible to match.
// 'endp' is pointer to the end of the pattern.
// Declaration of the main mehtod is needed since it is recursivly called.
static int match(const char* pat, const char* sam, const char* endp);

/****************************************************************************/
// Any char comparison is always true
static inline int c_any(	const char* pat, const char* sam){
	return 1;
}

/****************************************************************************/
// Single char comparison. Match uses one charactr. Wrong returns NO_MATCH.
static inline int c_achar(const char* pat, const char* sam){
	return (*pat == *sam) ? 1 : NO_MATCH;
}

/****************************************************************************/
static inline int c_group(	const char* pat, const char* sam){
	const char *close = compiledRegexPtr->getExpressionEnd_UnionSet(pat);
    if (!close)  return NO_MATCH;						// Could not match the paretheses. Wrong expresion. Exit
	int nCharsMatched;
	while (close[-1]=='|'){
		nCharsMatched = match(pat+1, sam, close-1);		// +1 and -1 remove the parentheses
		if (nCharsMatched >= 0)
			return nCharsMatched;
		pat   = close-1;								// Advance to the next alternative
		close = compiledRegexPtr->getExpressionEnd_UnionSet(pat);
		if (!close)  return NO_MATCH;					// Could not match the paretheses. Wrong expresion. Exit
	}
    return match(pat+1, sam, close-1);					// Execute the final alternative.
}

/****************************************************************************/
// All possible abbreviations
static inline int c_extended(	const char* pat, const char* sam){
	#define ABB_LENGTH  (32)
    char abbr[ABB_LENGTH] = "";    
    switch (*++pat){
        case 'd':	strcpy_s(abbr, ABB_LENGTH, "[0-9]");			break;	// Digit
        case 'D':   strcpy_s(abbr, ABB_LENGTH, "[^0-9]");			break;  // Non-digit
        case 'x':   strcpy_s(abbr, ABB_LENGTH, "[0-9A-Fa-f]");		break;	// Hex digit
        case 'X':	strcpy_s(abbr, ABB_LENGTH, "[^0-9A-Fa-f]");		break;	// Non Hex
        case 'w':   strcpy_s(abbr, ABB_LENGTH, "[0-9A-Za-z_]");		break;	// Word character
        case 'W':	strcpy_s(abbr, ABB_LENGTH, "[^0-9A-Za-z_]");	break;
        case 'h':   strcpy_s(abbr, ABB_LENGTH, "[0-9A-Za-z]");		break;	// head of word character
        case 'H':	strcpy_s(abbr, ABB_LENGTH, "[^0-9A-Za-z]");		break;
        case 'a':   strcpy_s(abbr, ABB_LENGTH, "[A-Za-z]");			break;	// Alphabetic character
        case 'A':	strcpy_s(abbr, ABB_LENGTH, "[^A-Za-z]");		break;
        case 'l':	strcpy_s(abbr, ABB_LENGTH, "[a-z]");			break;	// Lowercase character
        case 'L':	strcpy_s(abbr, ABB_LENGTH, "[^a-z]");			break;
        case 'u':   strcpy_s(abbr, ABB_LENGTH, "[A-Z]");			break;	// Uppercase character
        case 'U':	strcpy_s(abbr, ABB_LENGTH, "[^A-Z]");			break;
        case 's':   strcpy_s(abbr, ABB_LENGTH, "[ \t\r\n\v\f]");	break;	// Whitespace characters
        case 'S':	strcpy_s(abbr, ABB_LENGTH, "[^ \t\r\n\v\f]");	break;
    }

    if (*abbr)	return match(abbr, sam, endOfString(abbr));
    else		return c_achar(pat,sam);						// Unknown abbreviation. Just assume that it is a character comparison
}

/****************************************************************************/
// Chose one of the options in []. Like [\\-0-9$_#]
static inline  int c_option(	const char* pat, const char* sam){
    const char *from  = NULL;									// If we have [a-z]		'from' is 'a', 'to' is 'z'
    const char *to	  = NULL;									// If we have [qQ]		'from' is 'q' and 'Q', 'to' is not needed
	const char *close = compiledRegexPtr->getExpressionEnd(pat);// Extract the expression inside the [] parentheses
	pat++; close--;												// +1 and -1 remove the parentheses	
    int negationOp = ((*pat == '^') ? NO_MATCH : 1);			// Check for negation flag. Invert character [^a-z], representing negation operator
	if (negationOp<0)
		pat++;

    while (pat < close){		
        if (*pat == '-' && from){								// Check for range selection. Like 0-9, where we already have the from
			to = pat + 1;										// Find the 'to'
            if (*to == '\\')  to++;								// Comparison with reserved character. like \-  or \*			
			// Test for range			
            if is_int_inRange(*sam,*from,*to)				
                return negationOp;								// We have found a match. If 'not' is active than this is a violation of the pattern			
            pat = to + 1;										// So *sam didn't match the current range, try the next range. Like a-z and A-Z
            continue;
        }

        from = pat;												// Beggining of the pattern. Initialize 'from'
        if (*from == '\\'){ 
			from++; pat++;										// Comparison with reserved character. like \\* or \\?			
		}
		
        if (*sam == *from)
            return negationOp;									// Comparison of single letter. Like [a-ZAB]
        pat++;
    }
    return -negationOp;											// We tested all the options and nothing was mathing.  
}

/****************************************************************************/
// Multiple occurence of a character
static inline int c_multi(	const char* pat, const char* sam, const char* endp){
    const Cmd* cmd = get_cmd(*pat);
    int   nCharsMatched;														// How many characters the multi repitition consumed.
    int   nRestCharsMatched = NO_MATCH;											// How many characters the rest of the pattern consumes (if it exists).
    int   nRepitions;															// Counter, how many repititions were made up to now in a loop
    const char* start_sam = sam;
    const char *ends      = EOS;												// Get the end of the sample (stored in cache, instead of recalculation)
	const char *foundMatchAt = NULL;											// We already found a match but want to try and find a longer matching string
	const char *multi    = compiledRegexPtr->getExpressionEnd(pat);				// Multi occurence pattern: {}, *,?,+
    const char *next_pat = compiledRegexPtr->getExpressionEnd(multi);			// The rest of the pattern.

	// Calculate Min/Max numbers of needed occurences
    int min = 0, max = 1;														// Initialization not really needed. Just in case.	
    switch (*multi){
        case '{':  // For range of repetition: {4} or {4-8}
            {
                const char* comma = findFirstCinS(multi, ',');
				const char* rEnd  = findFirstCinS(multi, '}');
				// Read the minimum value
                min = atoui_simple(multi+1);
				// If comma exists inside {} than read also the maximum value
				if (comma){
					if       (comma <  rEnd-1)	max = MAX(atoui_simple(comma + 1),1);	// Read Max, Max must be at least 1;
					else if  (comma == rEnd-1)  max = (1<<30);					// Max does not exists: '{min,}', assume 1 billion is enough. Can use MAX_INT instead. Daniel did not want a dependency on <limits.h>
				}
				else                            max = MAX(min,1);				// No range: Like {4}, Max must be at least 1;
            } 
			break;
        case '+':		min = 1;		max = (1<<30);		break;
        case '?':		min = 0;		max = 1;			break;
        case '*':		min = 0;		max = (1<<30);		break;
    }

	// If (min==0), we first try to match the rest of pattern
	if ((min==0)&&(*next_pat)){
		nRestCharsMatched = match(next_pat, sam, endp);
		if (nRestCharsMatched>=0)			
			foundMatchAt = start_sam + nRestCharsMatched;	// Yes! The rest of the sample string matches the rest of the pattern. But maybe we can do more repititions and still be fine. like '.*b' matched to 'ab' but can also match  'abccqqb'
		// Note: if nRestCharsMatched==0 than the rest of the pattern can be matched to an empty string. Success is guaranteed. Now we want to match as much repititions as we can. Like 'a*b?' was matched to first character of 'aaaz' but can be matched to 'aaa'.
	}

	// OK. We need to take at least one repitiotion. Enter the loop
	nRepitions = 0;
    while (sam < ends){
        nCharsMatched = ((Stand_Func)cmd->f)(pat, sam);									// Find the pattern for the i'th time.
        if (nCharsMatched < 0){			
			// No more repetitions are possible
			if (nRepitions < min) return NO_MATCH;										// We need at least 'min' but failed

			if  (*next_pat){
				// If (nRestCharsMatched < 0)
				//      We have enough iterations but we already know that the rest of pattern can't be matched. If we found a good solution earlier return it. Otherwise no solution for matching
				// Else We have found a good solution right now and no more iterations possible. Return the good solution.
				return (foundMatchAt) ? (int)(foundMatchAt-start_sam) : NO_MATCH; 
			}
			return (int)(sam-start_sam);												// Macth found. Use 'nRepitions'
		}        
        sam += nCharsMatched;															// Found 'i' repitiotions. Advance pointers
        nRepitions++;

        if (nRepitions < min) continue;													// If we still havent reached the minimal amount of repititions than continue to gather more repetitions.
		
		// OK, we have at least 'min' iterations, Time to check the if the rest of the 
		// pattern can be matched. If not we will look for more occurences.
		// Otherwise we will use the current amount of occurences.
        if (*next_pat){
            nRestCharsMatched = match(next_pat, sam, endp);
            if (nRestCharsMatched>=0)
				foundMatchAt = sam + nRestCharsMatched;									// See explanation of the code line 'foundMatchAt = start_sam + nRestCharsMatched;' above
        }
		
	    if (nRepitions == max){															// Check the maximal limit of repititions.
			if  (*next_pat)
				return (foundMatchAt) ? (int)(foundMatchAt-start_sam) : NO_MATCH;		// See explanation for this exact code line above 
			return (int)(sam-start_sam);												// Macth found. Use maximal possible amounts of repitions.
		}
	}

    // None of the iterations yielded a consistent match. We exited the loop due to end of sample string.
    if (nRepitions < min) return NO_MATCH;												// Sample string terminated and we didn't get our minimal amount.

	if ((*next_pat)&&(nRestCharsMatched < 0))
		return (foundMatchAt) ? (int)(foundMatchAt-start_sam) : NO_MATCH;				// Sample string terminated and the rest of the pattern cannot be matched to an empty string. If we found a good solution return it. Otherwise no solution for matching	
    return (int)(sam-start_sam);														// No following patterns that require aditional characters and we got enough iterations. like a*z? on a string of 'aaaa'
}

/****************************************************************************/
// Match pattern to the 'sam' string from its beginning. 
// Returns the amount of consumed characters if match was successfull. Otherwise returns NO_MATCH.
// Note: 0 means successfull match. For example 'a?' is matched to 'bc' with zero occurences of 'a'
static int match(const char* pat, const char* sam, const char* endp){
    const Cmd* cmd;
    int  nCharsMatched;
    const char* start_sam = sam, *next_pat;

	if (!pat)
		return NO_MATCH;												// NULL pattern is illegal
    while (pat < endp){		
        next_pat  = compiledRegexPtr->getExpressionEnd(pat);			// Find next pattern to see if it is a suffix like *, {x,y},?,+
		if (next_pat==NULL)
			return NO_MATCH;											// Wrong regular expression. For example '(A)))'
		cmd = get_cmd(*next_pat);										// Check the next command if it is a suffix
		if (isSuffix(cmd)){												// 'cmd' is indeed a suffix. like 'z{3,7}'. Activate {3,7} on pattern 'z'.
			int matchedLen = ((SuffixFunc)cmd->f)(pat, sam, endp);		// Execute the suffix
			return (matchedLen>=0) ? (int)(sam-start_sam) + matchedLen : NO_MATCH;
        }
        else{															// No suffix
			cmd = get_cmd(*pat);
			// if (cmd->attr&TYPE_RECURSION){ To do: Handle the the case of the bug (.*)AB is not matched to 'ZAB' because .* consumes 3 letters
            nCharsMatched = ((Stand_Func)cmd->f)(pat, sam);
            if (nCharsMatched < 0) 
				return NO_MATCH;										// If matching failed return NO_MATCH.
			
            sam += nCharsMatched;										// Advance to next pattern
            pat = next_pat;
        }
    }
    return (int)(sam-start_sam);
}

/****************************************************************************/
/******************************** API methods *******************************/
/****************************************************************************/
const char* regex_search(const char* pattern, const char* sampleString, int* resLen){
	tCompiledRegex builtInCompiledRegex;
	builtInCompiledRegex.compile(pattern);
	return builtInCompiledRegex.search(sampleString, resLen);
}

/****************************************************************************/
const char* tCompiledRegex::search(const char* sampleString, int* resLen) const{
	compiledRegexPtr	   = this;										// Store 'this' as current regex
	const char* pattern    = start;
	const char* endPattern = end;
	const char* endOfSearch= EOS = endOfString(sampleString);			// When comparint the pattern to sample string we will search the entire sample.
	if (pattern == endPattern){											// Empty pattern is matched with zero length matching		
		*resLen = 0;
		return sampleString;
	}
	
	if (pattern[0]=='^'){												// Check if first characters forces a matching to beggining of the string
		pattern++;														// Skip the '^'
		endOfSearch = sampleString+1;									// Allow match only for first position.
	}

	// Try to match from every possible place in the sample string
    for (;sampleString != endOfSearch; sampleString++){
        *resLen = match(pattern, sampleString, endPattern);
        if (*resLen > 0)												// Note the >0 comparison and not >=. We do not allow empty string match.
            return sampleString;										// Full match was found. Return the current location in sample string.
    }
	
	sampleString = EOS;													// No match was found through the entire search.
	*resLen = match(pattern, sampleString, endPattern);					// Try to match pattern to empty string (since we didn't allow it before).
     return (*resLen>= 0) ? sampleString : NULL;						// Empty sample string was matched to pattern (like a*b*c*) (if==0). Otherwise no match
};

/****************************************************************************/
/****************************** UNITEST methods *****************************/
/****************************************************************************/
#ifdef REGEX_UNITEST
void regex_debug(const char* pattern, const char* sampleString, const char* trueAnswer){
	int   len = 0;
	char  output[256];
	const char* res;
	const char error[] = "@NO";
	const char empty[] = "";

	res =  regex_search(pattern, sampleString, &len);
	if (len>0){			memcpy(output,res, len); output[len] = '\0';	}
	else if (len==0)	memcpy(output,empty,strlen(empty)+1);
	else				memcpy(output,error,strlen(error)+1);	
	// Print only errors
	if (strcmp(output,trueAnswer))		fprintf(stderr,"Error: Reg:%s\t\t in %s\t\t : %s,\n", pattern, sampleString, output);
	else								fprintf(stderr,"Test OK!\n");
}

/****************************************************************************/
// Sample of regular expression and true answers. Checks that mechanism works fine
static void regex_debug_private_tests(void){
	regex_debug("(\\(*.\\[)[qQ]", "((a[Q]","((a[Q");
	regex_debug("((b)c)", "abc","bc");
	regex_debug("(a)((b)c)?", "abc","abc");
	regex_debug("(a)(((b))c)(d)?", "abc","abc");
	regex_debug("ba*(ac)?", "baaa","baaa");
	regex_debug("a{1,2}c?q?", "aaa","aa");
	regex_debug("a{3,}c?q?", "aaacq","aaacq");
	regex_debug("a{2}v", "aaav","aav");
	regex_debug("a*b*c*", "","");												// Testy empty matches
	regex_debug("a*","zb","");   
	regex_debug("a*b*c*q", "","@NO");
	regex_debug("ba*", "baaa","baaa");
	regex_debug("ba*(ac)+", "baaac","baaac");
	regex_debug("ba+", "baaa","baaa");
	regex_debug("ba{0,3}", "baaa","baaa");
	regex_debug("ba{1,2}c?q?", "baaa","baa");
	regex_debug("[0-3]*(22a)", "12222a","12222a");
	regex_debug("b[0-9]+", "abc123xyz","@NO");
	regex_debug("ac?.[0-2]*", "abc123xyz","ab");
	regex_debug("ac?b[q-z]*c{0,4}.[0-2]*(22a)", "abc12222a","abc12222a");
	regex_debug("(6(0|4|5)_)+", "61_60_64_65_A","60_64_65_");
	regex_debug("(6(0|4|5)_)*", "61_60_64_65_A","60_64_65_");
	regex_debug("(6(011|44|5)_)*", "6012_6011_644_65_6541_","6011_644_65_");
	regex_debug("^a[ \t\r\n\v\f]*", "za \n ","@NO");							// Test spaces
	regex_debug("a[ \t\r\n\v\f]*b", "ga \tbv","a \tb");
	regex_debug("a[ \t\r\n\v\f]*", "za \t\t \f\n ","a \t\t \f\n ");
	regex_debug("f..k","zfolky","folk");
	regex_debug("va*b","kkvaaaab","vaaaab");
	regex_debug(".*b","ababc","abab");
	regex_debug("z.*b","xyzababc","zabab");
	regex_debug(".*b","bc","b");
	regex_debug("[abc]1{2}(cat|pup+y|dog).{2}","_a11puppy11_","a11puppy11");   
	regex_debug("[abc]*\\|","zab|","ab|");   
	regex_debug("[abc]*\\|[AC-Z]*","zab|ACDB","ab|ACD");   
	regex_debug("[abc]*\\|[ABCK]*","zab|ACKvv","ab|ACK");   
	regex_debug("[ABCK\\+UZ0-9]*","AAZ+BUB","AAZ+BUB");   
	regex_debug("[ABCKUZ0-9\\-]*","AAZ-BUB","AAZ-BUB");			
	regex_debug("[ABCK\\-UZ0-9]*","AAZ-BUB","AAZ-BUB");							
	regex_debug("[abc]*\\|[A\\?B-Z]*","zab|A?BCD","ab|A?BCD");   
	regex_debug("[^xyz]","zab","a");   
	regex_debug("[^xyc]+","zab","zab");   
	regex_debug("([0-9]+a{2,4})+q","1aa23aaa445aaaaq","1aa23aaa445aaaaq");   
	regex_debug("([abc]?[01]?)*","a1b00aab","a1b00aab");   
	regex_debug("[a-c]+(x{2,4})*","cxxxxxxxx","cxxxxxxxx");   
	regex_debug("([a-c]+(x{1,2})?)+x","cxccxxccxxx","cxccxxccxxx");   
	regex_debug("a[0-9]+b",   "za789b","a789b");
	regex_debug("[^A-Za-z]*","abc12..","12..");
	regex_debug("^a\\+{3}\\*\\?b","a+++*?bc","a+++*?b");						// Check special characters
	regex_debug("\\[\\[","a[?][[1","[[");
	regex_debug("(V\\.?A\\.?T\\.?|TAX|TVA)( *[^A-Z]? +)?[A-_\\. ]*([0-9]+([\\.,][0-9]{1,2})?%?[0-9]+([\\.,][0-9]{1,2})?%_? +)?([A-_\\. ]*)?([0-9]+[\\.,][0-9]{1,2} +)?[A-_]? *[\\-\\+]?[0-9]+(,[0-9]{3})*[\\.,][0-9]{1,2} *.? *\n","         VAT                      3.93\n","VAT                      3.93\n");
	regex_debug("\\[\\]","a[?][]1","[]1");										
	regex_debug("(\\[\\?\\])*\\|(\\*)+","a[?][?]|**1","[?][?]|**");				
	regex_debug("(.+)(.+)","AB","AB");											// Known bug in Daniels implementation!
	regex_debug("((.*A)BABA)Z","ZABABABABAZQ","ZABABABABAZ");					// Known bug in Daniels implementation!
}

#endif
/****************************************************************************/
// EOF.