(**********************************************************´*******************
 *                     PUCU Pascal UniCode Utils Libary                       *
 ******************************************************************************
 *                        Version 2016-06-03-23-23-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pucu                                         *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 *                                                                            *
 ******************************************************************************)
unit PUCUCode;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpuamd64}
  {$define cpux86_64}
 {$endif}
 {$ifdef cpu386}
  {$define cpux86}
  {$define cpu32}
  {$asmmode intel}
 {$endif}
 {$ifdef cpux86_64}
  {$define cpux64}
  {$define cpu64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$ifdef cpux64}
  {$define cpux86_64}
  {$define cpu64}
 {$else}
  {$ifdef cpu386}
   {$define cpux86}
   {$define cpu32}
  {$endif}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

uses SysUtils,Classes,PUCUUnicodePass2;

const suDONOTKNOW=-1;
      suNOUTF8=0;
      suPOSSIBLEUTF8=1;
      suISUTF8=2;

      ucACCEPT=0;
      ucERROR=16;

type PPUCUQWord=^TPUCUQWord;
     PPUCUPtrUInt=^TPUCUPtrUInt;
     PPUCUPtrInt=^TPUCUPtrInt;

{$ifdef fpc}
     TPUCUQWord=qword;

     TPUCUPtrUInt=PtrUInt;
     TPUCUPtrInt=PtrInt;
{$else}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     TPUCUQWord=uint64;

     TPUCUPtrUInt=NativeUInt;
     TPUCUPtrInt=NativeInt;
{$else}
     TPUCUQWord=int64;

{$ifdef cpu64}
     TPUCUPtrUInt=TPUCUQWord;
     TPUCUPtrInt=int64;
{$else}
     TPUCUPtrUInt=longword;
     TPUCUPtrInt=longint;
{$endif}
{$ifend}
{$endif}

     PPUCUNativeUInt=^TPUCUNativeUInt;
     PPUCUNativeInt=^TPUCUNativeInt;
     TPUCUNativeUInt=TPUCUPtrUInt;
     TPUCUNativeInt=TPUCUPtrInt;

     PPUCURawByteChar=PAnsiChar;
     TPUCURawByteChar=ansichar;

     PPUCURawByteCharSet=^TPUCURawByteCharSet;
     TPUCURawByteCharSet=set of TPUCURawByteChar;

     TPUCURawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     TPUCUUTF8String={$ifdef HAS_TYPE_UTF8STRING}UTF8String{$else}AnsiString{$endif};

//>PUCUUnicodeData<//

function PUCUUnicodeGetCategoryFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetScriptFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsWord(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsIDBegin(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsIDPart(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsWhiteSpace(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToUpper(c:longword):longword; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToLower(c:longword):longword; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToTitle(c:longword):longword; {$ifdef caninline}inline;{$endif}

function PUCUUTF32CharToUTF8(CharValue:longword):TPUCURawByteString;
function PUCUUTF32CharToUTF8Len(CharValue:longword):longint;
function PUCUIsUTF8(const s:TPUCURawByteString):boolean;
function PUCUUTF8Validate(const s:TPUCURawByteString):boolean;
function PUCUUTF8Get(const s:TPUCURawByteString):longint;
function PUCUUTF8PtrGet(const s:PPUCURawByteChar;Len:longint):longint;
procedure PUCUUTF8SafeInc(const s:TPUCURawByteString;var CodeUnit:longint);
procedure PUCUUTF8PtrSafeInc(const s:PPUCURawByteChar;var Len,CodeUnit:longint);
procedure PUCUUTF8Inc(const s:TPUCURawByteString;var CodeUnit:longint);
procedure PUCUUTF8PtrInc(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint);
procedure PUCUUTF8Dec(const s:TPUCURawByteString;var CodeUnit:longint);
procedure PUCUUTF8PtrDec(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint);
procedure PUCUUTF8Delete(var s:TPUCURawByteString;CodeUnit:longint);
function PUCUUTF8Length(const s:TPUCURawByteString):longint;{$ifdef cpu386}assembler; register;{$endif}
function PUCUUTF8PtrLength(const s:TPUCURawByteString;Len:longint):longint;{$ifdef cpu386}assembler; register;{$endif}
function PUCUUTF8LengthEx(const s:TPUCURawByteString):longint;
function PUCUUTF8GetCodePoint(const s:TPUCURawByteString;CodeUnit:longint):longint;
function PUCUUTF8PtrGetCodePoint(const s:PPUCURawByteChar;Len,CodeUnit:longint):longint;
function PUCUUTF8GetCodeUnit(const s:TPUCURawByteString;CodePoint:longint):longint;
function PUCUUTF8PtrGetCodeUnit(const s:TPUCURawByteString;Len,CodePoint:longint):longint;
function PUCUUTF8CodeUnitGetChar(const s:TPUCURawByteString;CodeUnit:longint):longword;
function PUCUUTF8PtrCodeUnitGetChar(const s:PPUCURawByteChar;Len,CodeUnit:longint):longword;
function PUCUUTF8PtrCodeUnitGetCharFallback(const s:PPUCURawByteChar;Len,CodeUnit:longint):longword;
function PUCUUTF8CodeUnitGetCharAndInc(const s:TPUCURawByteString;var CodeUnit:longint):longword;
function PUCUUTF8PtrCodeUnitGetCharAndInc(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint):longword;
function PUCUUTF8CodeUnitGetCharFallback(const s:TPUCURawByteString;CodeUnit:longint):longword;
function PUCUUTF8CodeUnitGetCharAndIncFallback(const s:TPUCURawByteString;var CodeUnit:longint):longword;
function PUCUUTF8PtrCodeUnitGetCharAndIncFallback(const s:PPUCURawByteChar;const Len:longint;var CodeUnit:longint):longword;
function PUCUUTF8CodePointGetChar(const s:TPUCURawByteString;CodePoint:longint;Fallback:boolean=false):longword;
function PUCUUTF8GetCharLen(const s:TPUCURawByteString;i:longint):longword;
function PUCUUTF8Pos(const FindStr,InStr:TPUCURawByteString):longint;
function PUCUUTF8Copy(const Str:TPUCURawByteString;Start,Len:longint):TPUCURawByteString;
function PUCUUTF8UpperCase(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8LowerCase(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8Trim(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8Correct(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8FromLatin1(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8LevenshteinDistance(const s,t:TPUCURawByteString):longint;
function PUCUUTF8DamerauLevenshteinDistance(const s,t:TPUCURawByteString):longint;
function PUCUStringLength(const s:TPUCURawByteString):longint;

implementation

function PUCUUnicodeGetCategoryFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeCategoryArrayBlockBits;
  result:=PUCUUnicodeCategoryArrayBlockData[PUCUUnicodeCategoryArrayIndexBlockData[PUCUUnicodeCategoryArrayIndexIndexData[Index shr PUCUUnicodeCategoryArrayIndexBlockBits],Index and PUCUUnicodeCategoryArrayIndexBlockMask],c and PUCUUnicodeCategoryArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetScriptFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeScriptArrayBlockBits;
  result:=PUCUUnicodeScriptArrayBlockData[PUCUUnicodeScriptArrayIndexBlockData[PUCUUnicodeScriptArrayIndexIndexData[Index shr PUCUUnicodeScriptArrayIndexBlockBits],Index and PUCUUnicodeScriptArrayIndexBlockMask],c and PUCUUnicodeScriptArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeUpperCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeUpperCaseDeltaArrayBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeUpperCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeUpperCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeUpperCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeLowerCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeLowerCaseDeltaArrayBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeLowerCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeLowerCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeLowerCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeTitleCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeTitleCaseDeltaArrayBlockData[PUCUUnicodeTitleCaseDeltaArrayIndexBlockData[PUCUUnicodeTitleCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeTitleCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeTitleCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeTitleCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeIsWord(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNd,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsIDBegin(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsIDPart(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNd,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsWhiteSpace(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
//result:=UnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryZs,PUCUUnicodeCategoryZp,PUCUUnicodeCategoryZl];
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function PUCUUnicodeToUpper(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+PUCUUnicodeGetUpperCaseDeltaFromTable(c)));
end;

function PUCUUnicodeToLower(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+PUCUUnicodeGetLowerCaseDeltaFromTable(c)));
end;

function PUCUUnicodeToTitle(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+PUCUUnicodeGetTitleCaseDeltaFromTable(c)));
end;

function PUCUUTF32CharToUTF8(CharValue:longword):TPUCURawByteString;
var Data:array[0..{$ifdef PUCUStrictUTF8}3{$else}5{$endif}] of TPUCURawByteChar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=TPUCURawByteChar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=TPUCURawByteChar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef PUCUStrictUTF8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=TPUCURawByteChar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef PUCUStrictUTF8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=TPUCURawByteChar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=TPUCURawByteChar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,PPUCURawByteChar(@Data[0]),ResultLen);
 end;
end;

function PUCUUTF32CharToUTF8Len(CharValue:longword):longint;
begin
 if CharValue<=$7f then begin
  result:=1;
 end else if CharValue<=$7ff then begin
  result:=2;
 end else if CharValue<=$ffff then begin
  result:=3;
 end else if CharValue<=$1fffff then begin
  result:=4;
{$ifndef PUCUStrictUTF8}
 end else if CharValue<=$3ffffff then begin
  result:=5;
 end else if CharValue<=$7fffffff then begin
  result:=6;
{$endif}
 end else begin
  result:=3;
 end;
end;

function PUCUIsUTF8(const s:TPUCURawByteString):boolean;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=false;
    exit;
   end;
  end;
 end;
 result:=(State=ucACCEPT) and (length(s)<>CodePoints);
end;

function PUCUUTF8Validate(const s:TPUCURawByteString):boolean;
var CodeUnit:longint;
    State:longword;
begin
 State:=ucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
  if State=ucERROR then begin
   result:=false;
   exit;
  end;
 end;
 result:=State=ucACCEPT;
end;

function PUCUUTF8Get(const s:TPUCURawByteString):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=suNOUTF8;
    exit;
   end;
  end;
 end;
 if State=ucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=suISUTF8;
  end else begin
   result:=suPOSSIBLEUTF8;
  end;
 end else begin
  result:=suNOUTF8;
 end;
end;

function PUCUUTF8PtrGet(const s:PPUCURawByteChar;Len:longint):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=ucACCEPT;
 CodePoints:=0;
 for CodeUnit:=0 to Len-1 do begin
  State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(CodePoints);
   end;
   ucERROR:begin
    result:=suNOUTF8;
    exit;
   end;
  end;
 end;
 if State=ucACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=suISUTF8;
  end else begin
   result:=suPOSSIBLEUTF8;
  end;
 end else begin
  result:=suNOUTF8;
 end;
end;

procedure PUCUUTF8SafeInc(const s:TPUCURawByteString;var CodeUnit:longint);
var Len:longint;
    StartCodeUnit,State:longword;
begin
 Len:=length(s);
 if CodeUnit>0 then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure PUCUUTF8PtrSafeInc(const s:PPUCURawByteChar;var Len,CodeUnit:longint);
var StartCodeUnit,State:longword;
begin
 if CodeUnit>=0 then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure PUCUUTF8Inc(const s:TPUCURawByteString;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,PUCUUTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure PUCUUTF8PtrInc(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  inc(CodeUnit,PUCUUTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure PUCUUTF8Dec(const s:TPUCURawByteString;var CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=(length(s)+1)) then begin
  dec(CodeUnit);
  while CodeUnit>0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure PUCUUTF8PtrDec(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  dec(CodeUnit);
  while CodeUnit>=0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure PUCUUTF8Delete(var s:TPUCURawByteString;CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function PUCUUTF8Length(const s:TPUCURawByteString):longint; {$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,dword ptr [esi-4]
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function PUCUUTF8PtrLength(const s:TPUCURawByteString;Len:longint):longint;
{$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,edx
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=0 to Len-1 do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function PUCUUTF8LengthEx(const s:TPUCURawByteString):longint;
var State:longword;
    CodeUnit:longint;
begin
 result:=0;
 State:=ucACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=PUCUUTF8DFATransitions[State+PUCUUTF8DFACharClasses[s[CodeUnit]]];
  case State of
   ucACCEPT:begin
    inc(result);
   end;
   ucERROR:begin
    result:=0;
    exit;
   end;
  end;
 end;
 if State=ucERROR then begin
  result:=0;
 end;
end;

function PUCUUTF8GetCodePoint(const s:TPUCURawByteString;CodeUnit:longint):longint;
var CurrentCodeUnit,Len:longint;
begin
 if CodeUnit<1 then begin
  result:=-1;
 end else begin
  result:=0;
  CurrentCodeUnit:=1;
  Len:=length(s);
  while (CurrentCodeUnit<=Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,PUCUUTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function PUCUUTF8PtrGetCodePoint(const s:PPUCURawByteChar;Len,CodeUnit:longint):longint;
var CurrentCodeUnit:longint;
begin
 result:=-1;
 if CodeUnit<0 then begin
  CurrentCodeUnit:=0;
  while (CurrentCodeUnit<Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,PUCUUTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function PUCUUTF8GetCodeUnit(const s:TPUCURawByteString;CodePoint:longint):longint;
var CurrentCodePoint,Len:longint;
begin
 if CodePoint<0 then begin
  result:=0;
 end else begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<=Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,PUCUUTF8CharSteps[s[result]]);
  end;
 end;
end;

function PUCUUTF8PtrGetCodeUnit(const s:TPUCURawByteString;Len,CodePoint:longint):longint;
var CurrentCodePoint:longint;
begin
 result:=-1;
 if CodePoint>=0 then begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,PUCUUTF8CharSteps[s[result]]);
  end;
 end;
end;

function PUCUUTF8CodeUnitGetChar(const s:TPUCURawByteString;CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function PUCUUTF8PtrCodeUnitGetChar(const s:PPUCURawByteChar;Len,CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function PUCUUTF8PtrCodeUnitGetCharFallback(const s:PPUCURawByteChar;Len,CodeUnit:longint):longword;
var Value,CharClass,State:longword;
    StartCodeUnit:longint;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(TPUCURawByteChar(s[StartCodeUnit]));
  end;
 end;
end;

function PUCUUTF8CodeUnitGetCharAndInc(const s:TPUCURawByteString;var CodeUnit:longint):longword;
var Len:longint;
    Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function PUCUUTF8PtrCodeUnitGetCharAndInc(const s:PPUCURawByteChar;Len:longint;var CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function PUCUUTF8CodeUnitGetCharFallback(const s:TPUCURawByteString;CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(TPUCURawByteChar(s[StartCodeUnit]));
  end;
 end;
end;

function PUCUUTF8CodeUnitGetCharAndIncFallback(const s:TPUCURawByteString;var CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(TPUCURawByteChar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function PUCUUTF8PtrCodeUnitGetCharAndIncFallback(const s:PPUCURawByteChar;const Len:longint;var CodeUnit:longint):longword;
var StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=byte(TPUCURawByteChar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
   if State=ucACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=PUCUUTF8DFATransitions[State+CharClass];
   if State<=ucERROR then begin
    break;
   end;
  end;
  if State<>ucACCEPT then begin
   result:=byte(TPUCURawByteChar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function PUCUUTF8CodePointGetChar(const s:TPUCURawByteString;CodePoint:longint;Fallback:boolean=false):longword;
begin
 result:=PUCUUTF8CodeUnitGetChar(s,PUCUUTF8GetCodeUnit(s,CodePoint));
end;

function PUCUUTF8GetCharLen(const s:TPUCURawByteString;i:longint):longword;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=PUCUUTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function PUCUUTF8Pos(const FindStr,InStr:TPUCURawByteString):longint;
var i,j,l:longint;
    ok:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InStr) do begin
  l:=i+length(FindStr)-1;
  if l>length(InStr) then begin
   exit;
  end;
  ok:=true;
  for j:=1 to length(FindStr) do begin
   if InStr[i+j-1]<>FindStr[j] then begin
    ok:=false;
    break;
   end;
  end;
  if ok then begin
   result:=i;
   exit;
  end;
  inc(i,PUCUUTF8CharSteps[InStr[i]]);
 end;
end;

function PUCUUTF8Copy(const Str:TPUCURawByteString;Start,Len:longint):TPUCURawByteString;
var CodeUnit:longint;
begin
 result:='';
 CodeUnit:=1;
 while (CodeUnit<=length(Str)) and (Start>0) do begin
  inc(CodeUnit,PUCUUTF8CharSteps[Str[CodeUnit]]);
  dec(Start);
 end;
 if Start=0 then begin
  Start:=CodeUnit;
  while (CodeUnit<=length(Str)) and (Len>0) do begin
   inc(CodeUnit,PUCUUTF8CharSteps[Str[CodeUnit]]);
   dec(Len);
  end;
  if Start<CodeUnit then begin
   result:=copy(Str,Start,CodeUnit-Start);
  end;
 end;
end;

function PUCUUTF8UpperCase(const Str:TPUCURawByteString):TPUCURawByteString;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:PPUCURawByteChar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef PUCUStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(TPUCURawByteChar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=PUCUUTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr PUCUUnicodeUpperCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+PUCUUnicodeUpperCaseDeltaArrayBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexIndexData[Value shr PUCUUnicodeUpperCaseDeltaArrayIndexBlockBits],Value and PUCUUnicodeUpperCaseDeltaArrayIndexBlockMask],CharValue and PUCUUnicodeUpperCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function PUCUUTF8LowerCase(const Str:TPUCURawByteString):TPUCURawByteString;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:PPUCURawByteChar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef PUCUStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(TPUCURawByteChar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=PUCUUTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr PUCUUnicodeLowerCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+PUCUUnicodeLowerCaseDeltaArrayBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexIndexData[Value shr PUCUUnicodeLowerCaseDeltaArrayIndexBlockBits],Value and PUCUUnicodeLowerCaseDeltaArrayIndexBlockMask],CharValue and PUCUUnicodeLowerCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function PUCUUTF8Trim(const Str:TPUCURawByteString):TPUCURawByteString;
var i,j:longint;
begin
 i:=1;
 while PUCUUnicodeIsWhiteSpace(PUCUUTF8CodeUnitGetChar(Str,i)) do begin
  inc(i,PUCUUTF8CharSteps[Str[i]]);
 end;
 j:=length(Str)+1;
 PUCUUTF8Dec(Str,j);
 while PUCUUnicodeIsWhiteSpace(PUCUUTF8CodeUnitGetChar(Str,j)) do begin
  PUCUUTF8Dec(Str,j);
 end;
 if (j<=length(Str)) and (Str[j]>=#80) then begin
  inc(j,longint(PUCUUTF8GetCharLen(Str,j))-1);
 end;
 if i<=j then begin
  result:=copy(Str,i,(j-i)+1);
 end else begin
  result:='';
 end;
end;

function PUCUUTF8Correct(const Str:TPUCURawByteString):TPUCURawByteString;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:PPUCURawByteChar;
begin
 if (length(Str)=0) or PUCUUTF8Validate(Str) then begin
  result:=Str;
 end else begin
  result:='';
  CodeUnit:=1;
  Len:=length(Str);
  SetLength(result,Len*{$ifdef PUCUStrictUTF8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=ucACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(TPUCURawByteChar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=PUCUUTF8DFACharClasses[TPUCURawByteChar(Value)];
    if State=ucACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=PUCUUTF8DFATransitions[State+CharClass];
    if State<=ucERROR then begin
     break;
    end;
   end;
   if State<>ucACCEPT then begin
    CharValue:=byte(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);            
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function PUCUUTF8FromLatin1(const Str:TPUCURawByteString):TPUCURawByteString;
var CodeUnit:longint;
begin
 if PUCUUTF8Validate(Str) then begin
  result:=Str;
 end else begin
  result:='';
  for CodeUnit:=1 to length(Str) do begin
   result:=result+PUCUUTF32CharToUTF8(byte(TPUCURawByteChar(Str[CodeUnit])));
  end;
 end;
end;

function PUCUUTF8LevenshteinDistance(const s,t:TPUCURawByteString):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Deletion,Insertion,Substitution:longint;
    si,tj:longword;
begin
 n:=PUCUUTF8LengthEx(s);
 m:=PUCUUTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (PUCUUTF8CodeUnitGetChar(s,oi)=PUCUUTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,PUCUUTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,PUCUUTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  PUCUUTF8Dec(s,ci);
  PUCUUTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (PUCUUTF8CodeUnitGetChar(s,ci)=PUCUUTF8CodeUnitGetChar(t,cj)) do begin
   PUCUUTF8Dec(s,ci);
   PUCUUTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  for i:=1 to n do begin
   si:=PUCUUTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   for j:=1 to m do begin
    tj:=PUCUUTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Deletion:=d[i-1,j]+1;
     Insertion:=d[i,j-1]+1;
     Substitution:=d[i-1,j-1]+1;
     if Deletion<Insertion then begin
      if Deletion<Substitution then begin
       d[i,j]:=Deletion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end else begin
      if Insertion<Substitution then begin
       d[i,j]:=Insertion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end;
    end else begin
     d[i,j]:=d[i-1,j-1];
    end;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function PUCUUTF8DamerauLevenshteinDistance(const s,t:TPUCURawByteString):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Cost,Deletion,Insertion,Substitution,Transposition,Value:longint;
    si,tj,lsi,ltj:longword;
begin
 n:=PUCUUTF8LengthEx(s);
 m:=PUCUUTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (PUCUUTF8CodeUnitGetChar(s,oi)=PUCUUTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,PUCUUTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,PUCUUTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  PUCUUTF8Dec(s,ci);
  PUCUUTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (PUCUUTF8CodeUnitGetChar(s,ci)=PUCUUTF8CodeUnitGetChar(t,cj)) do begin
   PUCUUTF8Dec(s,ci);
   PUCUUTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  si:=0;
  for i:=1 to n do begin
   lsi:=si;
   si:=PUCUUTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   tj:=0;
   for j:=1 to m do begin
    ltj:=tj;
    tj:=PUCUUTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Cost:=1;
    end else begin
     Cost:=0;
    end;
    Deletion:=d[i-1,j]+1;
    Insertion:=d[i,j-1]+1;
    Substitution:=d[i-1,j-1]+Cost;
    if Deletion<Insertion then begin
     if Deletion<Substitution then begin
      Value:=Deletion;
     end else begin
      Value:=Substitution;
     end;
    end else begin
     if Insertion<Substitution then begin
      Value:=Insertion;
     end else begin
      Value:=Substitution;
     end;
    end;
    if ((i>1) and (j>1)) and ((si=ltj) and (lsi=tj)) then begin
     Transposition:=d[i-2,j-2]+Cost;
     if Transposition<Value then begin
      Value:=Transposition;
     end;
    end;
    d[i,j]:=Value;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function PUCUStringLength(const s:TPUCURawByteString):longint;
begin
 if PUCUIsUTF8(s) then begin
  result:=PUCUUTF8Length(s);
 end else begin
  result:=length(s);
 end;
end;

end.
