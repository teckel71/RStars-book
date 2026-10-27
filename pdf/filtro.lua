--[[
filtro.lua - Adapta el libro web a LaTeX/PDF sin tocar los .Rmd.
Solo actua cuando la salida es LaTeX (bookdown::pdf_book).

  1. Retira los iconos SVG de los titulos (decorativos; XeLaTeX no
     admite SVG y romperian el indice y los marcadores del PDF).
  2. Imprime la salida de consola de R en un entorno que parte las
     lineas largas en lugar de salirse del margen.
  3. Da a todas las tablas el mismo estilo (el de las tablas "de libro":
     filetes horizontales, sin rayas verticales y filas alternas en gris
     claro), las centra y las ajusta al ancho de la pagina (encabezados y
     celdas largas en varias lineas; si aun asi no caben, se reducen o,
     las muy anchas, se giran 90 grados).
  4. Centra las imagenes que no ocupan todo el ancho.
  5. Mantiene cada figura o tabla en la misma pagina que su rotulo
     ("Tabla X.Y." / "Figura X.Y.", que genera numerar_tablas_figuras.R).
     Si el conjunto no cabe en una pagina, se reduce lo justo.
  6. Convierte los desplegables <details> "sin MATrstars" en recuadros
     con su titulo.
  7. Omite la portada web (index.Rmd): el PDF tiene la suya propia
     (pdf/portada.tex).
]]

if not FORMAT:match('latex') then return {} end

local function latex(s) return pandoc.RawBlock('latex', s) end

-- Escapa los caracteres especiales de LaTeX
local especiales = {
  ['\\'] = '\\textbackslash{}', ['{'] = '\\{', ['}'] = '\\}',
  ['$'] = '\\$', ['&'] = '\\&', ['#'] = '\\#', ['%'] = '\\%',
  ['_'] = '\\_', ['^'] = '\\^{}', ['~'] = '\\textasciitilde{}',
}
local function escapar(s)
  return (s:gsub('[\\{}$&#%%_^~]', especiales))
end

local function ulen(s) return utf8.len(s) or #s end

-- 1. Iconos en titulos ----------------------------------------------------
function Header(h)
  local out = pandoc.List()
  for _, el in ipairs(h.content) do
    if el.t ~= 'Image' then out:insert(el) end
  end
  while #out > 0 and (out[1].t == 'Space' or out[1].t == 'SoftBreak') do
    out:remove(1)
  end
  h.content = out
  -- Capitulos sin numero (Prefacio, Bibliografia, Apendices...): que la
  -- cabecera de sus paginas muestre su titulo y no el del capitulo anterior.
  if h.level == 1 and #out > 0 and h.classes:includes('unnumbered') then
    local t = escapar(pandoc.utils.stringify(out))
    return { h, latex('\\markboth{' .. t .. '}{' .. t .. '}') }
  end
  return h
end

-- 2. Salida de consola de R (bloques de codigo sin lenguaje) ----------------
function CodeBlock(cb)
  if #cb.classes == 0 then
    return latex('\\begin{SalidaR}\n' .. cb.text .. '\n\\end{SalidaR}')
  end
end

-- 3. Tablas ------------------------------------------------------------------
local ANCHO_ENCABEZADO = 14   -- caracteres a partir de los cuales se parte
local ANCHO_CELDA      = 34   -- idem, para las celdas del cuerpo

local function ancho_columna(texto, minimo)
  local palabra = 0
  for w in texto:gmatch('%S+') do palabra = math.max(palabra, ulen(w)) end
  return math.max(minimo, palabra * 0.21)
end

-- Reparte en varias lineas encabezados y celdas largas (tablas de kable)
local function partir_celdas(s)
  -- \textbf{\multicolumn{n}{al}{x}} -> \multicolumn{n}{al}{\textbf{x}}
  -- (kableExtra genera el orden erroneo, que LaTeX no admite)
  local function reordenar(cmd, arg)
    local n, al, x = arg:sub(2, -2):match('^\\multicolumn(%b{})(%b{})(%b{})$')
    if n then return '\\multicolumn' .. n .. al .. '{\\' .. cmd .. x .. '}' end
  end
  s = s:gsub('\\(textbf)(%b{})', reordenar)
  s = s:gsub('\\(textit)(%b{})', reordenar)
  s = s:gsub('\\(emph)(%b{})', reordenar)

  s = s:gsub('\\multicolumn{1}{c}{\\textbf(%b{})}', function(arg)
    local texto = arg:sub(2, -2)
    if ulen(texto) <= ANCHO_ENCABEZADO or texto:find('\\\\') then return nil end
    return string.format(
      '\\multicolumn{1}{c}{\\parbox[b]{%.1fcm}{\\centering\\textbf{%s}}}',
      ancho_columna(texto, 2.3), texto)
  end)

  s = s:gsub('\\multicolumn{1}{c}(%b{})', function(arg)
    local texto = arg:sub(2, -2)
    if texto:sub(1, 1) == '\\' or ulen(texto) <= ANCHO_CELDA then return nil end
    -- unas 2-3 lineas de entre 4 y 6 cm
    local ancho = math.min(6, math.max(ancho_columna(texto, 4), ulen(texto) * 0.09))
    return string.format(
      '\\multicolumn{1}{c}{\\parbox[t]{%.1fcm}{\\raggedright %s}}',
      ancho, texto)
  end)
  return s
end

-- Filas alternas en gris claro. Si desde_inicio es falso, solo se colorean
-- las filas entre \midrule y \bottomrule (el cuerpo de la tabla). Una fila
-- puede ocupar varias lineas: el color va al principio de la fila.
local function cebra(lineas, desde_inicio)
  local en_cuerpo, inicio, n, out = desde_inicio, desde_inicio, 0, {}
  for _, l in ipairs(lineas) do
    if l ~= '\0' then
      if l:match('^\\midrule') then
        en_cuerpo, inicio = true, true
      elseif l:match('^\\bottomrule') then
        en_cuerpo = false
      elseif en_cuerpo and l:match('%S') then
        if inicio and not l:match('^\\rowcolor') and not l:match('^\\[a-z]*rule') then
          n = n + 1
          if n % 2 == 0 then l = '\\rowcolor{rstarsfila}' .. l end
        end
        inicio = l:match('\\\\%s*$') ~= nil
      end
      out[#out + 1] = l
    end
  end
  return table.concat(out, '\n')
end

-- Estilo comun: filetes de booktabs, sin rayas verticales, filas alternas
local function estilo_libro(tab)
  -- sin rayas verticales en la especificacion de columnas
  tab = tab:gsub('^(\\begin{tabular})(%b{})', function(b, spec)
    return b .. spec:gsub('|', '')
  end)

  local lineas = {}
  for l in (tab .. '\n'):gmatch('(.-)\n') do lineas[#lineas + 1] = l end

  local con_hline = tab:find('\\hline', 1, true) ~= nil
  local reglas = {}               -- posiciones de \hline
  if con_hline then
    for i, l in ipairs(lineas) do
      if l:match('^%s*\\hline%s*$') then reglas[#reglas + 1] = i end
    end
    if #reglas >= 2 then
      for k, i in ipairs(reglas) do
        if k == 1 then lineas[i] = '\\toprule'
        elseif k == 2 then lineas[i] = '\\midrule'
        elseif k == #reglas then lineas[i] = '\\bottomrule'
        else lineas[i] = '\0' end
      end
    end
  end

  return cebra(lineas, false)
end

-- Tabla de kable (LaTeX crudo de knitr): devuelve el LaTeX transformado
local function arreglar_tabla(s)
  s = s:gsub('\\rowcolor{gray!10}', '')             -- se pone mas abajo
  s = partir_celdas(s)
  s = s:gsub('(\\begin{tabular}.-\\end{tabular})', function(tab)
    return '\\tablarstars{' .. estilo_libro(tab) .. '}'
  end)
  -- sin entorno flotante: centrada, en su sitio
  s = s:gsub('\\begin{table}%b[]', '\\begin{table}')
  s = s:gsub('\\begin{table}', '\\begin{center}')
  s = s:gsub('\\end{table}', '\\end{center}')
  return s
end

local function es_tabla_cruda(b)
  return b.t == 'RawBlock' and (b.format == 'latex' or b.format == 'tex')
         and b.text:find('\\begin{tabular}', 1, true) ~= nil
end

-- Tabla de markdown (|...|). Si es corta, se convierte en tabular (para
-- que vaya en un bloque con su rotulo); si es larga (puede ocupar mas de
-- una pagina), sigue siendo longtable y el rotulo se mete en su pie, de
-- modo que siempre acompana a las ultimas filas de la tabla.
local FILAS_TABLA_LARGA = 14

local function a_latex(bloques)
  return (pandoc.write(pandoc.Pandoc(bloques), 'latex'):gsub('%s+$', ''))
end

local function filas_de(tbl)
  local n = 0
  for _, b in ipairs(tbl.bodies or {}) do n = n + #(b.body or {}) end
  return n
end

local function tabla_markdown(tbl, rotulo)
  if not pandoc.write then return nil end            -- pandoc antiguo
  local ok, tex = pcall(pandoc.write, pandoc.Pandoc({ tbl }), 'latex')
  if not ok or not tex:find('\\begin{longtable}', 1, true) then return nil end
  tex = tex:gsub('\\noalign{}', '')
  local _, fin_spec, spec = tex:find('\\begin{longtable}%b[](%b{})')
  if not spec then _, fin_spec, spec = tex:find('\\begin{longtable}(%b{})') end
  if not spec then return nil end
  local cuerpo = tex:sub(fin_spec + 1):match('^(.*)\\end{longtable}')
  if not cuerpo then return nil end
  local cab  = (cuerpo:match('^(.-)\\endhead') or ''):gsub('\\endfirsthead.*$', '')
  local pie  = cuerpo:match('\\endhead(.-)\\endlastfoot') or ''
  local filas = cuerpo:match('\\endlastfoot(.*)$') or cuerpo
  if filas_de(tbl) > FILAS_TABLA_LARGA then
    local lineas = {}
    for l in (filas .. '\n'):gmatch('(.-)\n') do lineas[#lineas + 1] = l end
    filas = cebra(lineas, true)
    local ncol = #(tbl.colspecs or {})
    local final = pie
    if rotulo and ncol > 0 then
      final = pie .. '\n\\multicolumn{' .. ncol .. '}{c}{\\rule{0pt}{1.6em}\\small ' ..
              a_latex(rotulo.content):gsub('\n', ' ') .. '}\\\\\n'
    end
    return latex('\\begingroup\\small\n\\begin{longtable}[]' .. spec .. '\n' ..
                 cab .. '\\endhead\n' .. final .. '\\endlastfoot\n' .. filas ..
                 '\n\\end{longtable}\n\\endgroup'), true
  end

  local tab = '\\begin{tabular}' .. spec .. '\n' .. cab .. filas .. pie ..
              '\n\\end{tabular}'
  tab = tab:gsub('\n%s*\n', '\n')
  return latex('\\begin{center}\\small\n\\tablarstars{' .. estilo_libro(tab) ..
               '}\n\\end{center}'), false
end

-- 4 a 7. Recorrido del documento --------------------------------------------
local function texto_html(s)
  return (s:gsub('<[^>]+>', ''):gsub('^%s+', ''):gsub('%s+$', ''))
end

local function titulo_desplegable(t)
  t = t:gsub('^%s*[Ss]in MATrstars%s*', '')
  if t == '' then t = 'Cómo se haría este paso escribiendo el código a mano' end
  return t
end

local function es_html(b, patron)
  return b.t == 'RawBlock' and b.format:match('html') and b.text:match(patron)
end

local function contiene_summary(b)
  if b.t ~= 'Plain' and b.t ~= 'Para' then return false end
  for _, el in ipairs(b.content) do
    if el.t == 'RawInline' and el.text:match('<summary') then return true end
  end
  return false
end

local function es_rotulo(b)
  return b and b.t == 'Div' and
         (b.attributes['style'] or ''):match('text%-align:%s*center') ~= nil
end

-- Parrafo formado solo por imagenes (p. ej. un grafico de R)
local function es_imagen(b)
  if not b or (b.t ~= 'Para' and b.t ~= 'Plain') then return false end
  local hay = false
  for _, el in ipairs(b.content) do
    if el.t == 'Image' then hay = true
    elseif el.t ~= 'Space' and el.t ~= 'SoftBreak' and el.t ~= 'LineBreak' then
      return false
    end
  end
  return hay
end

local function rotulo_latex(div)
  local out = pandoc.List({ latex('\\begin{rotulo}') })
  out:extend(div.content)
  out:insert(latex('\\end{rotulo}'))
  return out
end

local procesar   -- declaracion adelantada (recursiva)

procesar = function(bl)
  local out = pandoc.List()
  local i, n = 1, #bl
  while i <= n do
    local b = bl[i]

    if b.t == 'Header' and b.level == 1 and #b.content == 0 then
      -- 7. Portada web: se salta hasta el siguiente capitulo.
      i = i + 1
      while i <= n and not (bl[i].t == 'Header' and bl[i].level == 1) do
        i = i + 1
      end

    elseif es_html(b, '<details') then
      -- 6. Desplegables "sin MATrstars"
      local titulo, dentro = nil, pandoc.List()
      local resto = b.text:match('<summary.-</summary>')
      if resto then titulo = texto_html(resto) end
      local cerrado = b.text:match('</details>') ~= nil
      local en_summary = (not resto) and b.text:match('<summary') ~= nil
      i = i + 1
      while i <= n and not cerrado do
        local c = bl[i]
        if es_html(c, '</details>') then
          cerrado = true
        elseif es_html(c, '<summary') then
          local s = c.text:match('<summary.-</summary>')
          if s then titulo = texto_html(s) else en_summary = true end
        elseif en_summary and es_html(c, '</summary>') then
          en_summary = false
        elseif en_summary then
          titulo = (titulo or '') .. pandoc.utils.stringify(c)
        elseif contiene_summary(c) then
          titulo = pandoc.utils.stringify(c)
        else
          dentro:insert(c)
        end
        i = i + 1
      end
      out:insert(latex('\\begin{rstarspuro}{' ..
                       escapar(titulo_desplegable(titulo or '')) .. '}'))
      out:extend(procesar(dentro))
      out:insert(latex('\\end{rstarspuro}'))

    else
      -- 3 y 4. Tablas e imagenes
      local elemento, con_rotulo = nil, false
      if es_tabla_cruda(b) then
        elemento = latex(arreglar_tabla(b.text))
      elseif b.t == 'Table' then
        elemento, con_rotulo = tabla_markdown(b, es_rotulo(bl[i + 1]) and bl[i + 1] or nil)
      elseif es_imagen(b) then
        elemento = b
      end

      if elemento and con_rotulo then
        -- tabla larga que ya lleva el rotulo en su pie
        out:insert(elemento)
        i = i + (es_rotulo(bl[i + 1]) and 2 or 1)
      elseif elemento and es_rotulo(bl[i + 1]) then
        -- 5. Figura o tabla + rotulo, siempre en la misma pagina
        out:insert(latex('\\begin{rstarsbloque}'))
        out:insert(elemento)
        out:extend(rotulo_latex(bl[i + 1]))
        out:insert(latex('\\end{rstarsbloque}'))
        i = i + 2
      elseif elemento and es_imagen(b) then
        out:insert(latex('\\begin{center}'))
        out:insert(elemento)
        out:insert(latex('\\end{center}'))
        i = i + 1
      elseif elemento then
        out:insert(elemento)
        i = i + 1
      elseif es_rotulo(b) then
        out:extend(rotulo_latex(b))
        i = i + 1
      else
        out:insert(b)
        i = i + 1
      end
    end
  end
  return out
end

function Pandoc(doc)
  doc.blocks = procesar(doc.blocks)
  return doc
end
