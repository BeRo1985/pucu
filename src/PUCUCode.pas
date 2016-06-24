(******************************************************************************
 *                     PUCU Pascal UniCode Utils Libary                       *
 ******************************************************************************
 *                        Version 2016-06-24-22-25-0000                       *
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
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
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
 {$define CanInline}
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
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
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
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
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

type PPUCUInt8=^TPUCUInt8;
     TPUCUInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};

     PPUCUUInt8=^TPUCUUInt8;
     TPUCUUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};

     PPUCUInt16=^TPUCUInt16;
     TPUCUInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};

     PPUCUUInt16=^TPUCUUInt16;
     TPUCUUInt16={$ifdef fpc}UInt16{$else}Word{$endif};

     PPUCUInt32=^TPUCUInt32;
     TPUCUInt32={$ifdef fpc}Int32{$else}LongInt{$endif};

     PPUCUUInt32=^TPUCUUInt32;
     TPUCUUInt32={$ifdef fpc}UInt32{$else}LongWord{$endif};

     PPUCUInt64=^TPUCUInt64;
     TPUCUInt64=Int64;

     PPUCUUInt64=^TPUCUUInt64;
     TPUCUUInt64=UInt64;

     PPUCUPtrUInt=^TPUCUPtrUInt;
     PPUCUPtrInt=^TPUCUPtrInt;

{$ifdef fpc}
     TPUCUPtrUInt=PtrUInt;
     TPUCUPtrInt=PtrInt;
{$else}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     TPUCUPtrUInt=NativeUInt;
     TPUCUPtrInt=NativeInt;
{$else}
{$ifdef cpu64}
     TPUCUPtrUInt=UInt64;
     TPUCUPtrInt=int64;
{$else}
     TPUCUPtrUInt=TPUCUUInt32;
     TPUCUPtrInt=TPUCUInt32;
{$endif}
{$ifend}
{$endif}

     PPUCUNativeUInt=^TPUCUNativeUInt;
     PPUCUNativeInt=^TPUCUNativeInt;
     TPUCUNativeUInt=TPUCUPtrUInt;
     TPUCUNativeInt=TPUCUPtrInt;

     PPUCURawByteChar=PAnsiChar;
     TPUCURawByteChar=AnsiChar;

     PPUCURawByteCharSet=^TPUCURawByteCharSet;
     TPUCURawByteCharSet=set of TPUCURawByteChar;

     PPUCURawByteString=^TPUCURawByteString;
     TPUCURawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     PPUCUUTF8Char=PAnsiChar;
     TPUCUUTF8Char=AnsiChar;

     PPUCUUTF8String=^TPUCUUTF8String;
     TPUCUUTF8String={$ifdef HAS_TYPE_UTF8STRING}UTF8String{$else}AnsiString{$endif};

     PPUCUUTF16Char={$ifdef fpc}PUnicodeChar{$else}PWideChar{$endif};
     TPUCUUTF16Char={$ifdef fpc}UnicodeChar{$else}WideChar{$endif};

     PPUCUUTF16String=^TPUCUUTF16String;
     TPUCUUTF16String={$ifdef fpc}UnicodeString{$else}WideString{$endif};

     PPUCUUTF32Char=^TPUCUUTF32Char;
     TPUCUUTF32Char=TPUCUUInt32;

     TPUCUUTF32String=array of TPUCUUTF32Char;

//>PUCUUnicodeData<//

function PUCUUnicodeGetCategoryFromTable(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetScriptFromTable(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetUpperCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetLowerCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeGetTitleCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsWord(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsIDBegin(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsIDPart(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeIsWhiteSpace(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToUpper(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToLower(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
function PUCUUnicodeToTitle(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}

function PUCUIsUTF8(const s:TPUCURawByteString):boolean;
function PUCUUTF8Validate(const s:TPUCURawByteString):boolean;
function PUCUUTF8Get(const s:TPUCURawByteString):TPUCUInt32;
function PUCUUTF8PtrGet(const s:PPUCURawByteChar;Len:TPUCUInt32):TPUCUInt32;
procedure PUCUUTF8SafeInc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
procedure PUCUUTF8PtrSafeInc(const s:PPUCURawByteChar;var Len,CodeUnit:TPUCUInt32);
procedure PUCUUTF8Inc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
procedure PUCUUTF8PtrInc(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32);
procedure PUCUUTF8Dec(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
procedure PUCUUTF8PtrDec(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32);
procedure PUCUUTF8Delete(var s:TPUCURawByteString;CodeUnit:TPUCUInt32);
function PUCUUTF8Length(const s:TPUCURawByteString):TPUCUInt32;{$ifdef cpu386}assembler; register;{$endif}
function PUCUUTF8PtrLength(const s:TPUCURawByteString;Len:TPUCUInt32):TPUCUInt32;{$ifdef cpu386}assembler; register;{$endif}
function PUCUUTF8LengthEx(const s:TPUCURawByteString):TPUCUInt32;
function PUCUUTF8GetCodePoint(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUInt32;
function PUCUUTF8PtrGetCodePoint(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUInt32;
function PUCUUTF8GetCodeUnit(const s:TPUCURawByteString;CodePoint:TPUCUInt32):TPUCUInt32;
function PUCUUTF8PtrGetCodeUnit(const s:TPUCURawByteString;Len,CodePoint:TPUCUInt32):TPUCUInt32;
function PUCUUTF8CodeUnitGetChar(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8PtrCodeUnitGetChar(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8PtrCodeUnitGetCharFallback(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8CodeUnitGetCharAndInc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8PtrCodeUnitGetCharAndInc(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8CodeUnitGetCharFallback(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8CodeUnitGetCharAndIncFallback(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8PtrCodeUnitGetCharAndIncFallback(const s:PPUCURawByteChar;const Len:TPUCUInt32;var CodeUnit:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8CodePointGetChar(const s:TPUCURawByteString;CodePoint:TPUCUInt32;Fallback:boolean=false):TPUCUUInt32;
function PUCUUTF8GetCharLen(const s:TPUCURawByteString;i:TPUCUInt32):TPUCUUInt32;
function PUCUUTF8Pos(const FindStr,InStr:TPUCURawByteString):TPUCUInt32;
function PUCUUTF8Copy(const Str:TPUCURawByteString;Start,Len:TPUCUInt32):TPUCURawByteString;
function PUCUUTF8UpperCase(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8LowerCase(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8Trim(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8Correct(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8FromLatin1(const Str:TPUCURawByteString):TPUCURawByteString;
function PUCUUTF8LevenshteinDistance(const s,t:TPUCURawByteString):TPUCUInt32;
function PUCUUTF8DamerauLevenshteinDistance(const s,t:TPUCURawByteString):TPUCUInt32;
function PUCUStringLength(const s:TPUCURawByteString):TPUCUInt32;

function PUCUUTF16Correct(const Str:TPUCUUTF16String):TPUCUUTF16String;
function PUCUUTF16UpperCase(const Str:TPUCUUTF16String):TPUCUUTF16String;
function PUCUUTF16LowerCase(const Str:TPUCUUTF16String):TPUCUUTF16String;

function PUCUUTF32CharToUTF8(CharValue:TPUCUUTF32Char):TPUCURawByteString;
function PUCUUTF32CharToUTF8Len(CharValue:TPUCUUTF32Char):TPUCUInt32;

function PUCUUTF32ToUTF8(const s:TPUCUUTF32String):TPUCUUTF8String;
function PUCUUTF8ToUTF32(const s:TPUCUUTF8String):TPUCUUTF32String;

function PUCUUTF8ToUTF16(const s:TPUCUUTF8String):TPUCUUTF16STRING;
function PUCUUTF16ToUTF8(const s:TPUCUUTF16STRING):TPUCUUTF8String;

function PUCUUTF16ToUTF32(const Value:TPUCUUTF16String):TPUCUUTF32String;
function PUCUUTF32ToUTF16(const Value:TPUCUUTF32String):TPUCUUTF16String;

function PUCUUTF32CharToUTF16(const Value:TPUCUUTF32Char):TPUCUUTF16String;

implementation

function PUCUUnicodeGetCategoryFromTable(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
var Index:TPUCUUInt32;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeCategoryArrayBlockBits;
  result:=PUCUUnicodeCategoryArrayBlockData[PUCUUnicodeCategoryArrayIndexBlockData[PUCUUnicodeCategoryArrayIndexIndexData[Index shr PUCUUnicodeCategoryArrayIndexBlockBits],Index and PUCUUnicodeCategoryArrayIndexBlockMask],c and PUCUUnicodeCategoryArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetScriptFromTable(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
var Index:TPUCUUInt32;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeScriptArrayBlockBits;
  result:=PUCUUnicodeScriptArrayBlockData[PUCUUnicodeScriptArrayIndexBlockData[PUCUUnicodeScriptArrayIndexIndexData[Index shr PUCUUnicodeScriptArrayIndexBlockBits],Index and PUCUUnicodeScriptArrayIndexBlockMask],c and PUCUUnicodeScriptArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetUpperCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
var Index:TPUCUUInt32;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeUpperCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeUpperCaseDeltaArrayBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeUpperCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeUpperCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeUpperCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetLowerCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
var Index:TPUCUUInt32;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeLowerCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeLowerCaseDeltaArrayBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeLowerCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeLowerCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeLowerCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeGetTitleCaseDeltaFromTable(c:TPUCUUInt32):TPUCUInt32; {$ifdef caninline}inline;{$endif}
var Index:TPUCUUInt32;
begin
 if c<=$10ffff then begin
  Index:=c shr PUCUUnicodeTitleCaseDeltaArrayBlockBits;
  result:=PUCUUnicodeTitleCaseDeltaArrayBlockData[PUCUUnicodeTitleCaseDeltaArrayIndexBlockData[PUCUUnicodeTitleCaseDeltaArrayIndexIndexData[Index shr PUCUUnicodeTitleCaseDeltaArrayIndexBlockBits],Index and PUCUUnicodeTitleCaseDeltaArrayIndexBlockMask],c and PUCUUnicodeTitleCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function PUCUUnicodeIsWord(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNd,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsIDBegin(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsIDPart(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PUCUUnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryLu,PUCUUnicodeCategoryLl,PUCUUnicodeCategoryLt,PUCUUnicodeCategoryLm,PUCUUnicodeCategoryLo,PUCUUnicodeCategoryNd,PUCUUnicodeCategoryNl,PUCUUnicodeCategoryNo,PUCUUnicodeCategoryPc]) or (c=ord('_'));
end;

function PUCUUnicodeIsWhiteSpace(c:TPUCUUInt32):boolean; {$ifdef caninline}inline;{$endif}
begin
//result:=UnicodeGetCategoryFromTable(c) in [PUCUUnicodeCategoryZs,PUCUUnicodeCategoryZp,PUCUUnicodeCategoryZl];
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function PUCUUnicodeToUpper(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
begin
 result:=TPUCUUInt32(TPUCUInt32(TPUCUInt32(c)+PUCUUnicodeGetUpperCaseDeltaFromTable(c)));
end;

function PUCUUnicodeToLower(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
begin
 result:=TPUCUUInt32(TPUCUInt32(TPUCUInt32(c)+PUCUUnicodeGetLowerCaseDeltaFromTable(c)));
end;

function PUCUUnicodeToTitle(c:TPUCUUInt32):TPUCUUInt32; {$ifdef caninline}inline;{$endif}
begin
 result:=TPUCUUInt32(TPUCUInt32(TPUCUInt32(c)+PUCUUnicodeGetTitleCaseDeltaFromTable(c)));
end;

function PUCUIsUTF8(const s:TPUCURawByteString):boolean;
var CodeUnit,CodePoints:TPUCUInt32;
    State:TPUCUUInt32;
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
var CodeUnit:TPUCUInt32;
    State:TPUCUUInt32;
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

function PUCUUTF8Get(const s:TPUCURawByteString):TPUCUInt32;
var CodeUnit,CodePoints:TPUCUInt32;
    State:TPUCUUInt32;
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

function PUCUUTF8PtrGet(const s:PPUCURawByteChar;Len:TPUCUInt32):TPUCUInt32;
var CodeUnit,CodePoints:TPUCUInt32;
    State:TPUCUUInt32;
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

procedure PUCUUTF8SafeInc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
var Len:TPUCUInt32;
    StartCodeUnit,State:TPUCUUInt32;
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

procedure PUCUUTF8PtrSafeInc(const s:PPUCURawByteChar;var Len,CodeUnit:TPUCUInt32);
var StartCodeUnit,State:TPUCUUInt32;
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

procedure PUCUUTF8Inc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,PUCUUTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure PUCUUTF8PtrInc(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32);
begin
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  inc(CodeUnit,PUCUUTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure PUCUUTF8Dec(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32);
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

procedure PUCUUTF8PtrDec(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32);
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

procedure PUCUUTF8Delete(var s:TPUCURawByteString;CodeUnit:TPUCUInt32);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function PUCUUTF8Length(const s:TPUCURawByteString):TPUCUInt32; {$ifdef cpu386} assembler; register;
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
var CodeUnit:TPUCUInt32;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (TPUCUUInt8(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function PUCUUTF8PtrLength(const s:TPUCURawByteString;Len:TPUCUInt32):TPUCUInt32;
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
var CodeUnit:TPUCUInt32;
begin
 result:=0;
 for CodeUnit:=0 to Len-1 do begin
  if (TPUCUUInt8(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function PUCUUTF8LengthEx(const s:TPUCURawByteString):TPUCUInt32;
var State:TPUCUUInt32;
    CodeUnit:TPUCUInt32;
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

function PUCUUTF8GetCodePoint(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUInt32;
var CurrentCodeUnit,Len:TPUCUInt32;
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

function PUCUUTF8PtrGetCodePoint(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUInt32;
var CurrentCodeUnit:TPUCUInt32;
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

function PUCUUTF8GetCodeUnit(const s:TPUCURawByteString;CodePoint:TPUCUInt32):TPUCUInt32;
var CurrentCodePoint,Len:TPUCUInt32;
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

function PUCUUTF8PtrGetCodeUnit(const s:TPUCURawByteString;Len,CodePoint:TPUCUInt32):TPUCUInt32;
var CurrentCodePoint:TPUCUInt32;
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

function PUCUUTF8CodeUnitGetChar(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUUInt32;
var Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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

function PUCUUTF8PtrCodeUnitGetChar(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUUInt32;
var Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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

function PUCUUTF8PtrCodeUnitGetCharFallback(const s:PPUCURawByteChar;Len,CodeUnit:TPUCUInt32):TPUCUUInt32;
var Value,CharClass,State:TPUCUUInt32;
    StartCodeUnit:TPUCUInt32;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  for CodeUnit:=CodeUnit to Len-1 do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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
   result:=TPUCUUInt8(TPUCURawByteChar(s[StartCodeUnit]));
  end;
 end;
end;

function PUCUUTF8CodeUnitGetCharAndInc(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32):TPUCUUInt32;
var Len:TPUCUInt32;
    Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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

function PUCUUTF8PtrCodeUnitGetCharAndInc(const s:PPUCURawByteChar;Len:TPUCUInt32;var CodeUnit:TPUCUInt32):TPUCUUInt32;
var Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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

function PUCUUTF8CodeUnitGetCharFallback(const s:TPUCURawByteString;CodeUnit:TPUCUInt32):TPUCUUInt32;
var Len:TPUCUInt32;
    StartCodeUnit,Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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
   result:=TPUCUUInt8(TPUCURawByteChar(s[StartCodeUnit]));
  end;
 end;
end;

function PUCUUTF8CodeUnitGetCharAndIncFallback(const s:TPUCURawByteString;var CodeUnit:TPUCUInt32):TPUCUUInt32;
var Len:TPUCUInt32;
    StartCodeUnit,Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<=Len do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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
   result:=TPUCUUInt8(TPUCURawByteChar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function PUCUUTF8PtrCodeUnitGetCharAndIncFallback(const s:PPUCURawByteChar;const Len:TPUCUInt32;var CodeUnit:TPUCUInt32):TPUCUUInt32;
var StartCodeUnit,Value,CharClass,State:TPUCUUInt32;
begin
 result:=0;
 if (CodeUnit>=0) and (CodeUnit<Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=ucACCEPT;
  while CodeUnit<Len do begin
   Value:=TPUCUUInt8(TPUCURawByteChar(s[CodeUnit]));
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
   result:=TPUCUUInt8(TPUCURawByteChar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function PUCUUTF8CodePointGetChar(const s:TPUCURawByteString;CodePoint:TPUCUInt32;Fallback:boolean=false):TPUCUUInt32;
begin
 result:=PUCUUTF8CodeUnitGetChar(s,PUCUUTF8GetCodeUnit(s,CodePoint));
end;

function PUCUUTF8GetCharLen(const s:TPUCURawByteString;i:TPUCUInt32):TPUCUUInt32;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=PUCUUTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function PUCUUTF8Pos(const FindStr,InStr:TPUCURawByteString):TPUCUInt32;
var i,j,l:TPUCUInt32;
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

function PUCUUTF8Copy(const Str:TPUCURawByteString;Start,Len:TPUCUInt32):TPUCURawByteString;
var CodeUnit:TPUCUInt32;
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
var CodeUnit,Len,ResultLen:TPUCUInt32;
    StartCodeUnit,Value,CharClass,State,CharValue:TPUCUUInt32;
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
    Value:=TPUCUUInt8(TPUCURawByteChar(Str[CodeUnit]));
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
    CharValue:=TPUCUUInt8(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr PUCUUnicodeUpperCaseDeltaArrayBlockBits;
    CharValue:=TPUCUUInt32(TPUCUInt32(TPUCUInt32(CharValue)+PUCUUnicodeUpperCaseDeltaArrayBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexBlockData[PUCUUnicodeUpperCaseDeltaArrayIndexIndexData[Value shr PUCUUnicodeUpperCaseDeltaArrayIndexBlockBits],Value and PUCUUnicodeUpperCaseDeltaArrayIndexBlockMask],CharValue and PUCUUnicodeUpperCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
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
var CodeUnit,Len,ResultLen:TPUCUInt32;
    StartCodeUnit,Value,CharClass,State,CharValue:TPUCUUInt32;
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
    Value:=TPUCUUInt8(TPUCURawByteChar(Str[CodeUnit]));
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
    CharValue:=TPUCUUInt8(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr PUCUUnicodeLowerCaseDeltaArrayBlockBits;
    CharValue:=TPUCUUInt32(TPUCUInt32(TPUCUInt32(CharValue)+PUCUUnicodeLowerCaseDeltaArrayBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexBlockData[PUCUUnicodeLowerCaseDeltaArrayIndexIndexData[Value shr PUCUUnicodeLowerCaseDeltaArrayIndexBlockBits],Value and PUCUUnicodeLowerCaseDeltaArrayIndexBlockMask],CharValue and PUCUUnicodeLowerCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
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
var i,j:TPUCUInt32;
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
  inc(j,TPUCUInt32(PUCUUTF8GetCharLen(Str,j))-1);
 end;
 if i<=j then begin
  result:=copy(Str,i,(j-i)+1);
 end else begin
  result:='';
 end;
end;

function PUCUUTF8Correct(const Str:TPUCURawByteString):TPUCURawByteString;
var CodeUnit,Len,ResultLen:TPUCUInt32;
    StartCodeUnit,Value,CharClass,State,CharValue:TPUCUUInt32;
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
    Value:=TPUCUUInt8(TPUCURawByteChar(Str[CodeUnit]));
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
    CharValue:=TPUCUUInt8(TPUCURawByteChar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef PUCUStrictUTF8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef PUCUStrictUTF8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=TPUCURawByteChar(TPUCUUInt8($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
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
var CodeUnit:TPUCUInt32;
begin
 if PUCUUTF8Validate(Str) then begin
  result:=Str;
 end else begin
  result:='';
  for CodeUnit:=1 to length(Str) do begin
   result:=result+PUCUUTF32CharToUTF8(TPUCUUInt8(TPUCURawByteChar(Str[CodeUnit])));
  end;
 end;
end;

function PUCUUTF8LevenshteinDistance(const s,t:TPUCURawByteString):TPUCUInt32;
var d:array of array of TPUCUInt32;
    n,m,i,j,ci,cj,oi,oj,Deletion,Insertion,Substitution:TPUCUInt32;
    si,tj:TPUCUUInt32;
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

function PUCUUTF8DamerauLevenshteinDistance(const s,t:TPUCURawByteString):TPUCUInt32;
var d:array of array of TPUCUInt32;
    n,m,i,j,ci,cj,oi,oj,Cost,Deletion,Insertion,Substitution,Transposition,Value:TPUCUInt32;
    si,tj,lsi,ltj:TPUCUUInt32;
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

function PUCUStringLength(const s:TPUCURawByteString):TPUCUInt32;
begin
 if PUCUIsUTF8(s) then begin
  result:=PUCUUTF8Length(s);
 end else begin
  result:=length(s);
 end;
end;

function PUCUUTF16Correct(const Str:TPUCUUTF16String):TPUCUUTF16String;
var i,j:TPUCUInt32;
    w:TPUCUUTF32Char;
begin
 result:='';
 j:=0;
 i:=1;
 SetLength(result,length(Str)*2);
 while i<=length(Str) do begin
  w:=TPUCUUInt16(TPUCUUTF16Char(Str[i]));
  inc(i);
  if (i<=length(Str)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $fc00)=$dc00) then begin
   w:=(TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(w and $3ff)) shl 10) or TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $3ff))+$10000;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w shr 10) or $d800));
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
 SetLength(result,j);
end;

function PUCUUTF16UpperCase(const Str:TPUCUUTF16String):TPUCUUTF16String;
var i,j:TPUCUInt32;
    w:TPUCUUTF32Char;
begin
 result:='';
 j:=0;
 i:=1;
 SetLength(result,length(Str)*2);
 while i<=length(Str) do begin
  w:=TPUCUUInt16(TPUCUUTF16Char(Str[i]));
  inc(i);
  if (i<=length(Str)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $fc00)=$dc00) then begin
   w:=(TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(w and $3ff)) shl 10) or TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $3ff))+$10000;
   inc(i);
  end;
  w:=PUCUUnicodeToUpper(w);
  if w<=$d7ff then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w shr 10) or $d800));
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
 SetLength(result,j);
end;

function PUCUUTF16LowerCase(const Str:TPUCUUTF16String):TPUCUUTF16String;
var i,j:TPUCUInt32;
    w:TPUCUUTF32Char;
begin
 result:='';
 j:=0;
 i:=1;
 SetLength(result,length(Str)*2);
 while i<=length(Str) do begin
  w:=TPUCUUInt16(TPUCUUTF16Char(Str[i]));
  inc(i);
  if (i<=length(Str)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $fc00)=$dc00) then begin
   w:=(TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(w and $3ff)) shl 10) or TPUCUUTF32Char(TPUCUUInt16(TPUCUUTF16Char(Str[i])) and $3ff))+$10000;
   inc(i);
  end;
  w:=PUCUUnicodeToLower(w);
  if w<=$d7ff then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w shr 10) or $d800));
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
 SetLength(result,j);
end;

function PUCUUTF32CharToUTF8(CharValue:TPUCUUTF32Char):TPUCURawByteString;
var Data:array[0..{$ifdef PUCUStrictUTF8}3{$else}5{$endif}] of TPUCURawByteChar;
    ResultLen:TPUCUInt32;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef PUCUStrictUTF8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef PUCUStrictUTF8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=TPUCURawByteChar(TPUCUUInt8($fc or ((CharValue shr 30) and $01)));
   Data[1]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=TPUCURawByteChar(TPUCUUInt8($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=TPUCURawByteChar(TPUCUUInt8($80 or (CharValue and $3f)));
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

function PUCUUTF32CharToUTF8Len(CharValue:TPUCUUTF32Char):TPUCUInt32;
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

function PUCUUTF32ToUTF8(const s:TPUCUUTF32String):TPUCUUTF8String;
var i,j:TPUCUInt32;
    u4c:TPUCUUTF32Char;
begin
 result:='';
 j:=0;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
{$ifndef PUCUStrictUTF8}
  end else if u4c<=$3ffffff then begin
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   inc(j,6);
{$endif}
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   result[j]:=AnsiChar(TPUCUUInt8(u4c));
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=AnsiChar(TPUCUUInt8($c0 or ((u4c shr 6) and $1f)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($f0 or ((u4c shr 18) and $07)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 12) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 6) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,4);
{$ifndef PUCUStrictUTF8}
  end else if u4c<=$3ffffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($f8 or ((u4c shr 24) and $03)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 18) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 12) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 6) and $3f)));
   result[j+4]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($fc or ((u4c shr 30) and $01)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 24) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 18) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 12) and $3f)));
   result[j+4]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 6) and $3f)));
   result[j+5]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,6);
{$endif}
  end else begin
   u4c:=$fffd;
   result[j]:=AnsiChar(TPUCUUInt8($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or (u4c and $3f)));
   inc(j,3);
  end;
 end;
end;

function PUCUUTF8ToUTF32(const s:TPUCUUTF8String):TPUCUUTF32String;
var i,j:TPUCUInt32;
    b:TPUCUUInt8;
begin
 j:=0;
 i:=1;
 while i<=length(s) do begin
  b:=TPUCUUInt8(s[i]);
  if (b and $80)=0 then begin
   inc(i);
   inc(j);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) then begin
   inc(i,2);
   inc(j);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) then begin
   inc(i,3);
   inc(j);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) then begin
   inc(i,4);
   inc(j);
{$ifndef PUCUStrictUTF8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) then begin
   inc(i,5);
   inc(j);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) and ((TPUCUUInt8(s[i+5]) and $c0)=$80) then begin
   inc(i,6);
   inc(j);
{$endif}   
  end else begin
   inc(i);
   inc(j);
  end;
 end;
 SetLength(result,j);
 if j=0 then begin
  exit;
 end;
 j:=0;
 i:=1;
 while i<=length(s) do begin
  b:=TPUCUUInt8(s[i]);
  if (b and $80)=0 then begin
   result[j]:=b;
   inc(i);
   inc(j);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) then begin
   result[j]:=((TPUCUUInt8(s[i]) and $1f) shl 6) or (TPUCUUInt8(s[i+1]) and $3f);
   inc(i,2);
   inc(j);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) then begin
   result[j]:=((TPUCUUInt8(s[i]) and $0f) shl 12) or ((TPUCUUInt8(s[i+1]) and $3f) shl 6) or (TPUCUUInt8(s[i+2]) and $3f);
   inc(i,3);
   inc(j);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) then begin
   result[j]:=((TPUCUUInt8(s[i]) and $07) shl 18) or ((TPUCUUInt8(s[i+1]) and $3f) shl 12) or ((TPUCUUInt8(s[i+2]) and $3f) shl 6) or (TPUCUUInt8(s[i+3]) and $3f);
   inc(i,4);
   inc(j);
{$ifndef PUCUStrictUTF8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) then begin
   result[j]:=((TPUCUUInt8(s[i]) and $03) shl 24) or ((TPUCUUInt8(s[i+1]) and $3f) shl 18) or ((TPUCUUInt8(s[i+2]) and $3f) shl 12) or ((TPUCUUInt8(s[i+3]) and $3f) shl 6) or (TPUCUUInt8(s[i+4]) and $3f);
   inc(i,5);
   inc(j);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) and ((TPUCUUInt8(s[i+5]) and $c0)=$80) then begin
   result[j]:=((TPUCUUInt8(s[i]) and $01) shl 30) or ((TPUCUUInt8(s[i+1]) and $3f) shl 24) or ((TPUCUUInt8(s[i+2]) and $3f) shl 18) or ((TPUCUUInt8(s[i+3]) and $3f) shl 12) or ((TPUCUUInt8(s[i+4]) and $3f) shl 6) or (TPUCUUInt8(s[i+5]) and $3f);
   inc(i,6);
   inc(j);
{$endif}
  end else begin
   result[j]:=$fffd;
   inc(i);
   inc(j);
  end;
 end;
end;

function PUCUUTF8ToUTF16(const s:TPUCUUTF8String):TPUCUUTF16STRING;
var i,j:TPUCUInt32;
    w:TPUCUUInt32;
    b:TPUCUUInt8;
begin
 result:='';
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=TPUCUUInt8(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $1f) shl 6) or (TPUCUUInt8(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $0f) shl 12) or ((TPUCUUInt8(s[i+1]) and $3f) shl 6) or (TPUCUUInt8(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $07) shl 18) or ((TPUCUUInt8(s[i+1]) and $3f) shl 12) or ((TPUCUUInt8(s[i+2]) and $3f) shl 6) or (TPUCUUInt8(s[i+3]) and $3f);
   inc(i,4);
{$ifndef PUCUStrictUTF8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $03) shl 24) or ((TPUCUUInt8(s[i+1]) and $3f) shl 18) or ((TPUCUUInt8(s[i+2]) and $3f) shl 12) or ((TPUCUUInt8(s[i+3]) and $3f) shl 6) or (TPUCUUInt8(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) and ((TPUCUUInt8(s[i+5]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $01) shl 30) or ((TPUCUUInt8(s[i+1]) and $3f) shl 24) or ((TPUCUUInt8(s[i+2]) and $3f) shl 18) or ((TPUCUUInt8(s[i+3]) and $3f) shl 12) or ((TPUCUUInt8(s[i+4]) and $3f) shl 6) or (TPUCUUInt8(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
  end else if w<=$dfff then begin
   inc(j);
  end else if w<=$fffd then begin
   inc(j);
  end else if w<=$ffff then begin
   inc(j);
  end else if w<=$10ffff then begin
   inc(j,2);
  end else begin
   inc(j);
  end;
 end;
 SetLength(result,j);
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=TPUCUUInt8(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $1f) shl 6) or (TPUCUUInt8(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $0f) shl 12) or ((TPUCUUInt8(s[i+1]) and $3f) shl 6) or (TPUCUUInt8(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $07) shl 18) or ((TPUCUUInt8(s[i+1]) and $3f) shl 12) or ((TPUCUUInt8(s[i+2]) and $3f) shl 6) or (TPUCUUInt8(s[i+3]) and $3f);
   inc(i,4);
{$ifndef PUCUStrictUTF8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $03) shl 24) or ((TPUCUUInt8(s[i+1]) and $3f) shl 18) or ((TPUCUUInt8(s[i+2]) and $3f) shl 12) or ((TPUCUUInt8(s[i+3]) and $3f) shl 6) or (TPUCUUInt8(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((TPUCUUInt8(s[i+1]) and $c0)=$80) and ((TPUCUUInt8(s[i+2]) and $c0)=$80) and ((TPUCUUInt8(s[i+3]) and $c0)=$80) and ((TPUCUUInt8(s[i+4]) and $c0)=$80) and ((TPUCUUInt8(s[i+5]) and $c0)=$80) then begin
   w:=((TPUCUUInt8(s[i]) and $01) shl 30) or ((TPUCUUInt8(s[i+1]) and $3f) shl 24) or ((TPUCUUInt8(s[i+2]) and $3f) shl 18) or ((TPUCUUInt8(s[i+3]) and $3f) shl 12) or ((TPUCUUInt8(s[i+4]) and $3f) shl 6) or (TPUCUUInt8(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w shr 10) or $d800));
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
end;

function PUCUUTF16ToUTF8(const s:TPUCUUTF16STRING):TPUCUUTF8String;
var i,j:TPUCUInt32;
    w:TPUCUUInt32;
begin
 result:='';
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=TPUCUUInt16(s[i]);
  inc(i);
  if (i<=length(s)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(s[i])) and $fc00)=$dc00) then begin
   w:=(TPUCUUTF32Char(TPUCUUTF32Char(w and $3ff) shl 10) or TPUCUUTF32Char(TPUCUUInt16(s[i]) and $3ff))+$10000;
   inc(i);
  end;
  if w<=$7f then begin
   inc(j);
  end else if w<=$7ff then begin
   inc(j,2);
  end else if w<=$ffff then begin
   inc(j,3);
  end else if w<=$1fffff then begin
   inc(j,4);
{$ifndef PUCUStrictUTF8}
  end else if w<=$3ffffff then begin
   inc(j,5);
  end else if w<=$7fffffff then begin
   inc(j,6);
{$endif}
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 i:=1;
 while i<=length(s) do begin
  w:=TPUCUUInt16(s[i]);
  inc(i);
  if (i<=length(s)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(s[i])) and $fc00)=$dc00) then begin
   w:=(TPUCUUTF32Char(TPUCUUTF32Char(w and $3ff) shl 10) or TPUCUUTF32Char(TPUCUUInt16(s[i]) and $3ff))+$10000;
   inc(i);
  end;
  if w<=$7f then begin
   result[j]:=AnsiChar(TPUCUUInt8(w));
   inc(j);
  end else if w<=$7ff then begin
   result[j]:=AnsiChar(TPUCUUInt8($c0 or ((w shr 6) and $1f)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,2);
  end else if w<=$ffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($e0 or ((w shr 12) and $0f)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((w shr 6) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,3);
  end else if w<=$1fffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($f0 or ((w shr 18) and $07)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((w shr 12) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((w shr 6) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,4);
{$ifndef PUCUStrictUTF8}
  end else if w<=$3ffffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($f8 or ((w shr 24) and $03)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((w shr 18) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((w shr 12) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or ((w shr 6) and $3f)));
   result[j+4]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,5);
  end else if w<=$7fffffff then begin
   result[j]:=AnsiChar(TPUCUUInt8($fc or ((w shr 30) and $01)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((w shr 24) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or ((w shr 18) and $3f)));
   result[j+3]:=AnsiChar(TPUCUUInt8($80 or ((w shr 12) and $3f)));
   result[j+4]:=AnsiChar(TPUCUUInt8($80 or ((w shr 6) and $3f)));
   result[j+5]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,6);
{$endif}
  end else begin
   w:=$fffd;
   result[j]:=AnsiChar(TPUCUUInt8($e0 or (w shr 12)));
   result[j+1]:=AnsiChar(TPUCUUInt8($80 or ((w shr 6) and $3f)));
   result[j+2]:=AnsiChar(TPUCUUInt8($80 or (w and $3f)));
   inc(j,3);
  end;
 end;
end;

function PUCUUTF16ToUTF32(const Value:TPUCUUTF16String):TPUCUUTF32String;
var i,j:TPUCUInt32;
    w:TPUCUUInt32;
begin
 i:=1;
 j:=0;
 result:=nil;
 try
  SetLength(result,length(Value));
  while i<=length(Value) do begin
   w:=TPUCUUInt16(Value[i]);
   inc(i);
   if (i<=length(Value)) and ((w and $fc00)=$d800) and ((TPUCUUInt16(TPUCUUTF16Char(Value[i])) and $fc00)=$dc00) then begin
    w:=(TPUCUUTF32Char(TPUCUUTF32Char(w and $3ff) shl 10) or TPUCUUTF32Char(TPUCUUInt16(Value[i]) and $3ff))+$10000;
    inc(i);
   end;
   result[j]:=w;
   inc(j);
  end;
 finally
  SetLength(result,j);
 end;
end;

function PUCUUTF32ToUTF16(const Value:TPUCUUTF32String):TPUCUUTF16String;
var i,j:TPUCUInt32;
    w:TPUCUUInt32;
begin
 result:='';
 j:=0;
 for i:=0 to length(Value)-1 do begin
  w:=Value[i];
  if w<=$d7ff then begin
   inc(j);
  end else if w<=$dfff then begin
   inc(j);
  end else if w<=$fffd then begin
   inc(j);
  end else if w<=$ffff then begin
   inc(j);
  end else if w<=$10ffff then begin
   inc(j,2);
  end else begin
   inc(j);
  end;
 end;
 SetLength(result,j);
 j:=0;
 for i:=0 to length(Value)-1 do begin
  w:=Value[i];
  if w<=$d7ff then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w shr 10) or $d800));
   inc(j);
   result[j]:=TPUCUUTF16Char(TPUCUUInt16((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
end;

function PUCUUTF32CharToUTF16(const Value:TPUCUUTF32Char):TPUCUUTF16String;
begin
 if Value<=$d7ff then begin
  result:=TPUCUUTF16Char(TPUCUUInt16(Value));
 end else if Value<=$dfff then begin
  result:=#$fffd;
 end else if Value<=$fffd then begin
  result:=TPUCUUTF16Char(TPUCUUInt16(Value));
 end else if Value<=$ffff then begin
  result:=#$fffd;
 end else if Value<=$10ffff then begin
  result:=TPUCUUTF16Char(TPUCUUInt16(((Value-$10000) shr 10) or $d800));
  result:=result+TPUCUUTF16Char(TPUCUUInt16(((Value-$10000) and $3ff) or $dc00));
 end else begin
  result:=#$fffd;
 end;
end;

end.
