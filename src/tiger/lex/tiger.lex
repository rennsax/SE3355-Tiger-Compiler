%filenames = "scanner"

 /*
  * Please don't modify the lines above.
  */

 /* You can add lex definitions here. */

%x COMMENT STR IGNORE STR_ESCAPE_SEQ

%%

 /* reserved words */
"array"                   {adjust(); return Parser::ARRAY;}
"while"                   {adjust(); return Parser::WHILE;}
"for"                     {adjust(); return Parser::FOR;}
"to"                      {adjust(); return Parser::TO;}
"break"                   {adjust(); return Parser::BREAK;}
"let"                     {adjust(); return Parser::LET;}
"in"                      {adjust(); return Parser::IN;}
"end"                     {adjust(); return Parser::END;}
"function"                {adjust(); return Parser::FUNCTION;}
"var"                     {adjust(); return Parser::VAR;}
"type"                    {adjust(); return Parser::TYPE;}
"if"                      {adjust(); return Parser::IF;}
"then"                    {adjust(); return Parser::THEN;}
"else"                    {adjust(); return Parser::ELSE;}
"do"                      {adjust(); return Parser::DO;}
"of"                      {adjust(); return Parser::OF;}
"nil"                     {adjust(); return Parser::NIL;}

 // Identifier
[_[:alpha:]][_[:alnum:]]* {adjust(); return Parser::ID;}
[[:digit:]]+   {adjust(); return Parser::INT;}

 /* Punctuation */
":="                      {adjust(); return Parser::ASSIGN;}
"<>"                      {adjust(); return Parser::NEQ;}
"<="                      {adjust(); return Parser::LE;}
">="                      {adjust(); return Parser::GE;}
,                         {adjust(); return Parser::COMMA;}
;                         {adjust(); return Parser::SEMICOLON;}
:                         {adjust(); return Parser::COLON;}
\(                        {adjust(); return Parser::LPAREN;}
\)                        {adjust(); return Parser::RPAREN;}
\[                        {adjust(); return Parser::LBRACK;}
\]                        {adjust(); return Parser::RBRACK;}
\{                        {adjust(); return Parser::LBRACE;}
\}                        {adjust(); return Parser::RBRACE;}
\.                        {adjust(); return Parser::DOT;}
"+"                       {adjust(); return Parser::PLUS;}
"-"                       {adjust(); return Parser::MINUS;}
"/"                       {adjust(); return Parser::DIVIDE;}
"*"                       {adjust(); return Parser::TIMES;}
=                         {adjust(); return Parser::EQ;}
"<"                       {adjust(); return Parser::LT;}
">"                       {adjust(); return Parser::GT;}
"&"                       {adjust(); return Parser::AND;}
"|"                       {adjust(); return Parser::OR;}

 /* string literal */
\"                        {
  errormsg_->tok_pos_ = char_pos_;
  begin(StartCondition__::STR);
  string_buf_act_len_ = 0;
}
<STR>\\                   {
  begin(StartCondition__::STR_ESCAPE_SEQ);
  ++string_buf_act_len_;
}
<STR_ESCAPE_SEQ>["\\]     {
  ++string_buf_act_len_;
  string_buf_.append(matched());
  begin(StartCondition__::STR);
}
<STR_ESCAPE_SEQ>[n]       {
  ++string_buf_act_len_;
  string_buf_.append(1, '\n');
  begin(StartCondition__::STR);
}
<STR_ESCAPE_SEQ>[t]       {
  ++string_buf_act_len_;
  string_buf_.append(1, '\t');
  begin(StartCondition__::STR);
}
<STR_ESCAPE_SEQ>([[:digit:]]{3})          {
  string_buf_act_len_ += 3;
  auto ascii = std::stoi(matched());
  string_buf_.append(1, ascii);
  begin(StartCondition__::STR);
}
<STR_ESCAPE_SEQ>(\^[@[:upper:]\[\]\\\^_]) {
  string_buf_act_len_ += 2;
  auto ascii = matched()[1] - '@';
  string_buf_.append(1, ascii);
  begin(StartCondition__::STR);
}
<STR_ESCAPE_SEQ>\n                        {
  errormsg_->Newline();
  begin(StartCondition__::IGNORE);
  string_buf_act_len_++;
}
 // isprint: match printable character or space
<IGNORE>[^[:print:]]{+}[ ]                {
  string_buf_act_len_++;
}
<IGNORE>\\                {
  string_buf_act_len_++;
  begin(StartCondition__::STR);
}
<IGNORE>.                 {
  _LEXER_ERROR("illegal character in the escape sequence");
}
<STR_ESCAPE_SEQ>.         {
  _LEXER_ERROR("unknown escape sequence");
}

 // The end of string literal
<STR>\"                   {
  errormsg_->tok_pos_ = char_pos_;
  char_pos_ += string_buf_act_len_ + 2;
  begin(StartCondition__::INITIAL);
  setMatched(string_buf_);
  string_buf_.clear();
  return Parser::STRING;
}
<STR>[[:print:] ]         {
  string_buf_act_len_++;
  string_buf_.append(matched());
}
<STR>.                    {
  _LEXER_ERROR("invalid character in string literal");
}

 /* comment */

"/*"                      {
  adjust();
  _COM_LVL_INC();
  begin(StartCondition__::COMMENT);
}
<COMMENT>"/*"             {
  adjust();
  _COM_LVL_INC();
}
<COMMENT>\n               {
  adjust();
  errormsg_->Newline();
}
<COMMENT>"*/"             {
  adjust();
  _COM_LVL_DEC();
  if (_COM_LVL_GET() == 1) begin(StartCondition__::INITIAL);
}
<COMMENT><<EOF>>          {
  adjust();
  _LEXER_ERROR("unexpected end of file");
}
<COMMENT>.                { adjust(); }

 /*
  * skip white space chars.
  * space, tabs and LF
  */
[ \t]+ {adjust();}
\n {adjust(); errormsg_->Newline();}

 /* illegal input */
. {adjust(); _LEXER_ERROR("illegal token");}
