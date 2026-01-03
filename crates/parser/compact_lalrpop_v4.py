#!/usr/bin/env python3
import re
import sys
from collections import defaultdict

# Regex mejorada para limpiar ANSI y caracteres extraños
ANSI_ESCAPE = re.compile(r'\x1B(?:\[[0-?]*[ -/]*[@-~]|\(.|[@-Z\\-_])')

def clean(text):
    if not text: return ""
    return " ".join(text.split()).strip()

def strip_ansi(text):
    return ANSI_ESCAPE.sub('', text)

def parse_errors(content):
    # REGEX FLEXIBLE: Busca "CUALQUIER_COSA:linea:col: linea:col: TIPO detected"
    # Esto evita problemas con rutas absolutas o relativas.
    header_pattern = r'(\S+:\d+:\d+:\s+\d+:\d+:\s+(?:Local ambiguity|Ambiguous grammar|Conflict) detected)'
    
    parts = re.split(header_pattern, content)
    
    if len(parts) < 2:
        print(f"DEBUG: No se encontró el patrón de LALRPOP. Revisa si el log tiene el formato 'file:line:col'.")
        return []

    parsed_results = []
    
    # parts[0] es el texto antes del primer error.
    # parts[i] es el header, parts[i+1] es el cuerpo.
    for i in range(1, len(parts), 2):
        header = parts[i]
        body = parts[i+1] if i+1 < len(parts) else ""
        
        # Extraer línea
        line_match = re.search(r':(\d+):', header)
        line = line_match.group(1) if line_match else "0"
        
        # --- CONFLICTOS ---
        if "Conflict detected" in header:
            # Buscamos el token y la regla en disputa
            conflict_match = re.search(r'looking at a token `([^`]+)` we can reduce to a `([^`]+)` but we can also (shift|reduce to a `([^`]+)`)', body)
            if conflict_match:
                token = conflict_match.group(1)
                reduce_to = conflict_match.group(2)
                alternative = conflict_match.group(3)
                parsed_results.append({
                    'line': line,
                    'type': 'Conflict',
                    'key': f"{reduce_to} vs {alternative}",
                    'token': f"`{token}`"
                })

        # --- AMBIGÜEDAD LOCAL ---
        elif "Local ambiguity detected" in header:
            obs = re.search(r'observed the following symbols:?\s*(.*?)\s*At that point', body, re.DOTALL)
            look = re.search(r'next token is a\s*`([^`]+)`', body)
            op_a = re.search(r'First,.*?produce a `([^`]+)`', body, re.DOTALL)
            op_b = re.search(r'Alternatively,.*?(?:construct|produce) a `([^`]+)`', body, re.DOTALL)
            
            b_str = f"Construct `{op_b.group(1)}`" if op_b else ("Shift token" if "shift the" in body else "Unknown")
            ctx = clean(obs.group(1)) if obs else "?"
            tkn = look.group(1) if look else "?"
            rule_a = op_a.group(1) if op_a else "?"
            
            msg = f"Ambiguity: {ctx} • `{tkn}` | A: Produce `{rule_a}` | B: {b_str}"
            parsed_results.append({'line': line, 'type': 'Ambiguity', 'msg': msg})

        # --- GRAMÁTICA AMBIGUA ---
        elif "Ambiguous grammar detected" in header:
            syms = re.search(r'symbols can be reduced in two ways:?\s*\n(.*?)\n\s*First', body, re.DOTALL)
            s_list = clean(syms.group(1)) if syms else "?"
            parsed_results.append({'line': line, 'type': 'Grammar', 'msg': f"Grammar: {s_list} can be reduced in 2+ ways"})

    return parsed_results

def main():
    if len(sys.argv) < 2:
        print("Uso: python compact_lalrpop_v4.py log.txt")
        return

    try:
        with open(sys.argv[1], 'r', encoding='utf-8', errors='replace') as f:
            content = strip_ansi(f.read())
    except Exception as e:
        print(f"Error al leer archivo: {e}")
        return

    raw_data = parse_errors(content)
    
    if not raw_data:
        print("--- NO SE ENCONTRARON ERRORES ---")
        return

    final_output = defaultdict(lambda: defaultdict(list))
    for item in raw_data:
        if item['type'] == 'Conflict':
            final_output[item['line']][item['key']].append(item['token'])
        else:
            final_output[item['line']][item['msg']].append(None)

    print(f"--- LALRPOP SEMANTIC REPORT ({len(raw_data)} items) ---")
    for line in sorted(final_output.keys(), key=int):
        print(f"L{line}:")
        for key, tokens in final_output[line].items():
            if tokens[0] is not None:
                t_str = ", ".join(sorted(set(tokens)))
                print(f"  Conflict: Tokens [{t_str}] -> Reduce `{key}`")
            else:
                print(f"  {key}")

if __name__ == "__main__":
    main()