" Various Vim utils to make it easier to build API wrappers.
"

map <leader>xp mckdd$blye'a/])<cr>bla '',<esc>hP'b^<c-a>^yt<bar>'cPa<esc>0wfpcw:<esc>wiFCur.<esc>f:li= FieldValue.As   <esc>hdiwwbelct;<esc>/property<cr>


" Convert tab separated list (copied from API Documentation) of properties into a TXEROPropertyEntry
map <leader>xd :s/^\(\s*\)\<\(\k\+\)\>\t\([^	]*\)/( PropType: xptString;     PropName: '\2';                   PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), \/\/ \3\r/<cr>

" Convert a Xero enum to a getter/setter
map <leader>xe :s/\(\<TXERO\(\k\+\)\)/function XERO\2AsEnum( stg : String) : \1;\rvar\r  idx : \1;\rbegin\r  result := low(\1);\r  for idx := low(idx) to high(idx) do\r  begin\r    if CompareText(stg, CXERO\2Type[idx]) = 0 then\r    begin\r      result := idx;\r      break;\r    end;\r  end;\rend;\rfunction XERO\2AsString( enumVal : Integer) : String;\rbegin\r  if (enumVal < 0) or (enumVal > ord(high(\1))) then\r    enumVal := low(\1);\r  result := CXERO\2Type[\1(enumVal)];\rend;\r<CR>

" Property to property entry in const list
map <leader>xP :s/\s*property\s\+\(\k\+\)\>\s*:\s*\(\k\+\>\)\s\+index\s\+\([^ ]*\) .*$/( PropType: xpt\2;  PropName: '\1';            PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate];),/<CR>

" Convert lhs of tab separated item into a TXEROPropertyEntry
fun! XeroTabToPropEntry()
  s/\(^\|\t\)\(\s*\)\<\(\k\+\)\>\t\([^	]*\)/( PropType: xptString;     PropName: '\2';                   PropDefault: ''; PropUsage: [xpuReqNew, xpuReqUpdate]; ), \/\/ \3\r/g
endfun

" ( PropType: xptEnum;     PropName: 'Type';                   PropDefault: ''; PropUsage : []; ), // See Invoice Types

" Create a pretty fleshed out implementation of an object from a list of prop
" type entries (above). Make sure all the property Types are set first.
fun! XeroPropEntryToProp( name) range
  let classname='TXERO'.a:name
  let enumname=classname.'Field'
  let pref='x'.tolower(substitute(a:name, '\U','','g')).'f'
  let enums=[]
  let rawmembers=[]
  let newlines=[]
  let enumtoint=[]
  let enumtostr=[]
  let idx=line('''<')
  let start=idx
  let finish= line('''>')
  let enumgetset=[]
  let getobjimpl=[]
  let getlistimpl=[]
  let destructimpl=[]

  let funcs= [
    \ '  public',
    \ '    class function PropTypeIndex( AField : Word) : TXEROPropertyMapEntry; override;',
    \ '    class function PropInfo( AField : Word ) : TXEROPropertyEntry; override;',
    \ '    class function PropTypeCount( APropType : TXEROPropertyType) : integer; override;',
    \ '    class function PropFieldID( AFieldName : String) : integer; override;',
    \ '    class function PropObjectName : String; override;',
    \ '    class function PropSpecialFieldID( Field : TXEROSpecialField) : integer; override;']
  let enumfuncs= [
    \  '    class function PropStringAsEnum( AField : Word; const StgVal : String) : integer; override;',
    \  '    class function PropEnumAsString( AField : Word; IntVal : Integer) : string; override;' ]
  let objfuncs=[
    \  '    function GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject; override;' ]
  let listfuncs=[
    \ '    function GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase; override;' ]

  let hasenum=0
  let hasobj=0
  let haslist=0
  let impl= []

  call add(newlines,'  published')
  let mx='^\s*(\s*PropType\s*:\s*xpt\(\k\+\)\s*\%({\s*\(\k\+\)\s*}\)\=;\s*PropName\s*:\s*''\([^'']*\)''\%(.\{-}//\s*\(.*\)$\)\='
  while idx <= finish
    let m=matchlist(getline(idx),mx)
    if empty(m) || m[0] == ''
      call add(newlines, getline(idx))
    else
      let type=m[1]
      let getter='r'.type.'Val'
      let setter='w'.type.'Val'
      let propname=m[3]
      let enum=pref.propname
      let overridetype=m[2]
      let comment=m[4]
      call add(enums, enum)
      let index=' index '.enum
      if type=='List'
        let haslist=1
        if overridetype == ''
          let type='TXERO'.propname.'List'
        else
          let type=overridetype
        endif
        let getter='r'.propname.'List'
        let setter='w'.propname.'List'
        let index=''
        let raw='F'.propname.'List'
        call add(rawmembers, '    '.raw.' : '.type.';')
        call add(destructimpl, '  FreeAndNil('.raw.');')
        call add(enumgetset,
          \'    function '.getter.' : '.type.';')
        call add(enumgetset,
          \'    procedure '.setter.'(ANewVal : '.type.');')

        call extend(impl, [
          \'function '.classname.'.'.getter.' : '.type.';',
          \'begin',
          \'  if not assigned('.raw.') then',
          \'    '.raw.' := '.type.'.Create;',
          \'  result := '.raw,
          \'end;',
          \'procedure '.classname.'.'.setter.'(ANewVal : '.type.');',
          \'begin',
          \'  if assigned(ANewVal) then',
          \'    '.getter.'.Assign(ANewVal)',
          \'  else if Assigned('.raw.') then',
          \'    '.raw.'.Clear;',
          \'end;'])
        call extend(getlistimpl, [
              \'    ord('.enum.'):',
              \'      case access of',
              \'        xfamWrite: result := '.getter.';',
              \'        xfamClear:',
              \'          begin',
              \'            FreeAndNil('.raw.');',
              \'            result := nil',
              \'          end;',
              \'      else result := '.raw.';',
              \'      end;'] )

      elseif type=='Object'
        let hasobj=1
        if overridetype == ''
          let type='TXERO'.propname
        else
          let type=overridetype
        endif

        let getter='r'.propname
        let setter='w'.propname
        let index=''
        let raw='F'.propname
        call add(rawmembers, '    '.raw.' : '.type.';')
        call add(destructimpl, '  FreeAndNil('.raw.');')

        call add(enumgetset,
          \'    function '.getter.' : '.type.';')
        call add(enumgetset,
          \'    procedure '.setter.'(ANewVal : '.type.');')

        call extend(impl, [
          \'function '.classname.'.'.getter.' : '.type.';',
          \'begin',
          \'  if not assigned('.raw.') then',
          \'    '.raw.' := '.type.'.Create;',
          \'  result := '.raw,
          \'end;',
          \'procedure '.classname.'.'.setter.'(ANewVal : '.type.');',
          \'begin',
          \'  if assigned(ANewVal) then',
          \'    '.getter.'.Assign(ANewVal)',
          \'  else if Assigned('.raw.') then',
          \'    '.raw.'.Clear;',
          \'end;'])
        call add(getobjimpl,
          \'    ord('.enum.'):',
          \'      case Access of',
          \'        xfamWrite: result := '.getter.';',
          \'        xfamClear:',
          \'          begin',
          \'            FreeAndNil('.raw.');',
          \'            result := nil;',
          \'          end;',
          \'      else',
          \'        result := '.raw.';',
          \'      end;')

      elseif type=='Enum'
        let hasenum=1
        if overridetype == ''
          let type='TXERO'.propname
        else
          let type=overridetype
        endif

        let getter='r'.propname.'Enum'
        let setter='w'.propname.'Enum'
        call add(enumtoint,
          \'    ord('.enum.'): result := ord(XERO'.propname.'AsEnum(StgVal));')
        call add(enumtostr,
          \'    ord('.enum.'): result := XERO'.propname.'AsString(intVal);')
        call add(enumgetset,
          \'    function '.getter.'(AField : integer) : '.type.';')
        call add(enumgetset,
          \'    procedure '.setter.'(AField : integer; ANewVal : '.type.');')

        call extend(impl, [
          \'function '.classname.'.'.getter.'(AField : integer) : '.type.';',
          \'var',
          \'  idx : integer;',
          \'begin',
          \'  idx := rIntegerVal(AField);',
          \'  if (idx < 0) or (idx > ord(high('.type.'))) then',
          \'    idx := 0;',
          \'  result := '.type.'(idx);',
          \'end;',
          \'procedure '.classname.'.'.setter.'(AField : integer; ANewVal : '.type.');',
          \'begin',
          \'  wIntegerVal(AField, ord(ANewVal));',
          \'end;'])

      elseif type=='DateTime'
        let type='T'.type
      endif

      if comment != ''
        call add(newlines, '    // '.comment)
      endif
      let newline='    property '.propname.' : '.type.index.' read '.getter.' write '.setter.';'
      call add(newlines,newline)
    endif
    let idx+=1
  endwhile

  '<,'> delete

  let expandname=substitute(a:name, '\(\l\)\(\u\)', '\1 \2','g')

  call extend(impl, [
    \ 'class function '.classname.'.PropObjectName : string;',
    \ 'begin',
    \ '  result := '''.a:name.''';',
    \ 'end;',
    \ 'class function '.classname.'.PropTypeIndex( AField : Word) : TXEROPropertyMapEntry;',
    \ 'begin',
    \ '  if AField > high(G_'.a:name.'Map.Map) then',
    \ '    Raise Exception.CreateFmt(''Invalid '.expandname.' Field %d'', [AField]);',
    \ '  result := G_'.a:name.'Map.Map[AField];',
    \ 'end;',
    \ '',
    \ 'class function '.classname.'.PropInfo( AField : Word ) : TXEROPropertyEntry;',
    \ 'begin',
    \ '  if AField > high(G_'.a:name.'Map.Map) then',
    \ '    Raise Exception.CreateFmt(''Invalid '.expandname.' Field %d'', [AField]);',
    \ '  result := C'.a:name.'Properties[AField];',
    \ 'end;',
    \ '',
    \ 'class function '.classname.'.PropTypeCount( APropType : TXEROPropertyType) : integer;',
    \ 'begin',
    \ '  result := G_'.a:name.'Map.Count[APropType];',
    \ 'end;',
    \ '',
    \ 'class function '.classname.'.PropFieldID( AFieldName : String) : integer;',
    \ 'begin',
    \ '  result := G_'.a:name.'Map.NameToFieldID(AFieldName);',
    \ 'end;',
    \ '',
    \ 'class function '.classname.'.PropSpecialFieldID( Field : TXEROSpecialField) : integer;',
    \ 'begin',
    \ '  case field of',
    \ '    xsfUID: result := ord('.pref.a:name.'ID);',
    \ '    xsfName:result := ord('.pref.'Name);',
    \ '  else      result := inherited PropSpecialFieldID(field);',
    \ '  end;',
    \ 'end;' ])

  if hasenum
    call extend(impl,  [
      \ 'class function '.classname.'.PropStringAsEnum( AField : Word; const StgVal : String) : integer;',
      \ 'begin',
      \ '  case AField of'])
    call extend(impl,enumtoint)
    call extend(impl,  [
      \ '  else result := inherited PropStringAsEnum(AField, StgVal);',
      \ '  end;',
      \ 'end;',
      \ ''])
    call extend(impl, [
      \ 'class function '.classname.'.PropEnumAsString( AField : Word; IntVal : Integer) : string;',
      \ 'begin',
      \ '  case AField of'])
    call extend(impl, enumtostr)
    call extend(impl, [
      \ '  else result := inherited PropEnumAsString(AField, IntVal);',
      \ '  end;',
      \ 'end;',
      \ ''])
  endif

  if hasobj
    call extend(impl, [
    \  'function '.classname.'.GetObject(field : Integer; Access : TXEROFieldAccessMode) :TXEROObject;',
    \  'begin',
    \  '  case field of' ])
    call extend(impl, getobjimpl)
    call extend(impl, [
    \  '  else',
    \  '    result := inherited GetObject(field, Access);',
    \  '  end',
    \  'end;', '' ])
  endif
  if haslist
    call extend(impl, [
    \  'function '.classname.'.GetListObject(Field : Integer; Access : TXEROFieldAccessMode) : TXEROObjectListBase;',
    \  'begin',
    \  '  case field of' ])
    call extend(impl, getlistimpl)
    call extend(impl, [
    \  '  else',
    \  '    result := inherited GetListObject(field, Access);',
    \  '  end',
    \  'end;', '' ])
  endif
  if !empty(destructimpl)
    call extend(impl, [
      \  'procedure '.classname.'.BeforeDestruction;',
      \  'begin'])

    call extend(impl, destructimpl)
    call extend(impl, [
      \  '  inherited BeforeDestruction;',
      \  'end;', '' ])
  endif

  call add(newlines, '  end;')
  call extend(newlines, [
        \ '  '. classname.'List = class(TXEROObjectList<'.classname.'>)',
        \ '  public',
        \ '    class function PropListName : String; override;',
        \ '  end;'])
  call extend(impl, [ '',
        \ 'class function '.classname.'List.PropListName : String;',
        \ 'begin',
        \ '  result :=  '''.a:name.'s''; // TODO Check',
        \ 'end;', ''])

  call append(start-1, impl)

  call append(start-1, newlines)
  if !empty(destructimpl)
    call append(start-1, ['  public','    procedure BeforeDestruction; override;' ])
  endif
  if haslist
    call append(start-1, listfuncs)
  endif
  if hasobj
    call append(start-1, objfuncs)
  endif
  call append(start-1, enumgetset)
  if !empty(rawmembers)
    call append(start-1, rawmembers)
  endif
  if !empty(rawmembers) || haslist || hasobj || hasenum
    call append(start-1, '  protected')
  endif

  if hasenum
    call append(start-1, enumfuncs)
  endif
  call append(start-1, funcs)

  call append(start-1,'  '.classname.' = class(TXEROObject)')
  call append(start-1, '  '.enumname.' = ('.join(enums, ', ').');')

endfun
