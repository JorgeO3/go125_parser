#!/usr/bin/env python3
import re
import sys
import argparse
from collections import defaultdict

# 1. Regex MEJORADA para ANSI:
#    - \x1B matches ESC
#    - (?: ... ) grupo no captura
#    - \[[0-?]*[ -/]*[@-~]  -> Detecta secuencias CSI estándar (colores tipo [31m)
#    - \(.                  -> Detecta cambios de charset (como (B ) que rompían el script anterior
#    - [@-Z\\-_]            -> Otras secuencias de escape
ANSI_ESCAPE = re.compile(r'\x1B(?:\[[0-?]*[ -/]*[@-~]|\(.|[@-Z\\-_])')

def strip_ansi(text):
    return ANSI_ESCAPE.sub('', text)

def parse_lalrpop_output(content):
    # 2. Regex MEJORADA para cabeceras:
    #    - Usa \s+ (uno o más espacios) en lugar de un espacio fijo, por si quedan huecos tras limpiar.
    #    - Busca patrones flexibles de Local ambiguity, Ambiguous grammar o Conflict.
    header_regex = r'(src/grammar\.lalrpop:\d+:\d+:\s+\d+:\d+:\s+(?:Local ambiguity|Ambiguous grammar|Conflict) detected)'
    
    # Usamos split para trocear el archivo usando las cabeceras como separador
    parts = re.split(header_regex, content)
    
    # parts[0] es basura inicial.
    # A partir de ahí: parts[1]=Cabecera1, parts[2]=Cuerpo1, parts[3]=Cabecera2, parts[4]=Cuerpo2...
    errors = []
    
    # Recorremos de 2 en 2
    for i in range(1, len(parts), 2):
        header = parts[i].strip()
        body = parts[i+1] if i+1 < len(parts) else ""
        
        # Extraer número de línea
        line_match = re.search(r':(\d+):', header)
        line_num = int(line_match.group(1)) if line_match else 0
        
        error_type = "Unknown"
        if "Local ambiguity" in header:
            error_type = "Local Ambiguity"
        elif "Ambiguous grammar" in header:
            error_type = "Ambiguous Grammar"
        elif "Conflict" in header:
            error_type = "Conflict"
            
        errors.append({
            'line': line_num,
            'type': error_type,
            'header': header,
            'body': body
        })
        
    return errors

def extract_context(error):
    body = error['body']
    info = {}
    
    if error['type'] == "Local Ambiguity":
        # Contexto observado
        obs_match = re.search(r'observed the following symbols.*?\n(.*?)\n\s*At that point', body, re.DOTALL)
        if obs_match:
            info['observed'] = " ".join(obs_match.group(1).split())
            
        # Token siguiente (Lookahead)
        look_match = re.search(r'next token is a\s*`([^`]+)`', body)
        if look_match:
            info['lookahead'] = look_match.group(1)
            
        # Opción 1
        first_match = re.search(r'First, the parser could.*?(produce a `[^`]+`|execute the production at)', body, re.DOTALL)
        if first_match:
            raw_op1 = first_match.group(1)
            # Limpiar un poco más si dice "execute the production..."
            if "execute" in raw_op1:
                # Intentar buscar qué produce esa regla
                prod_match = re.search(r'produce a `([^`]+)`', body)
                if prod_match: raw_op1 = f"produce a `{prod_match.group(1)}`"
            info['option1'] = " ".join(raw_op1.split())

        # Opción 2
        alt_match = re.search(r'Alternatively, the parser could.*?(construct a `[^`]+`|shift the `[^`]+` token)', body, re.DOTALL)
        if alt_match:
            info['option2'] = " ".join(alt_match.group(1).split())
            
    elif error['type'] == "Conflict":
        # Buscar conflicto reduce/shift o reduce/reduce
        conflict_match = re.search(r'looking at a token `([^`]+)` we can reduce to a `([^`]+)` but we can also (shift|reduce)', body)
        if conflict_match:
            info['token'] = conflict_match.group(1)
            info['reduce_to'] = conflict_match.group(2)
            info['conflict_action'] = conflict_match.group(3)
            info['desc'] = f"Token `{info['token']}`: Reduce to `{info['reduce_to']}` VS {info['conflict_action']}"
        else:
            # Intento genérico para conflictos complejos
            info['desc'] = "Conflicto complejo (revisar body original)"

    elif error['type'] == "Ambiguous Grammar":
        sym_match = re.search(r'The following symbols can be reduced in two ways:\s*\n(.*?)\n', body, re.DOTALL)
        if sym_match:
            info['symbols'] = " ".join(sym_match.group(1).split())
            
    return info

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("file", help="Path to output.txt")
    args = parser.parse_args()

    try:
        # errors='replace' es vital para no fallar con bytes extraños
        with open(args.file, 'r', encoding='utf-8', errors='replace') as f:
            raw_content = f.read()
    except FileNotFoundError:
        print(f"Error: No encuentro el archivo '{args.file}'")
        sys.exit(1)

    clean_content = strip_ansi(raw_content)
    
    # DEBUG: Si sigue fallando, descomentar esto para ver cómo quedó el texto limpio
    # with open("debug_cleaned.txt", "w") as f: f.write(clean_content)

    errors = parse_lalrpop_output(clean_content)
    
    if not errors:
        print("ADVERTENCIA: No se encontraron errores. Posibles causas:")
        print("1. El archivo está vacío o no es un log de LALRPOP.")
        print("2. El formato de Regex 'src/grammar.lalrpop' no coincide con tu nombre de archivo real.")
        print("   (Revisa si tu archivo se llama diferente dentro del log).")
        sys.exit(0)

    # Agrupar y ordenar
    errors_by_line = defaultdict(list)
    for err in errors:
        errors_by_line[err['line']].append(err)
        
    sorted_lines = sorted(errors_by_line.keys())
    
    print(f"{'='*80}")
    print(f" RESUMEN DE ERRORES LALRPOP ({len(errors)} errores encontrados)")
    print(f"{'='*80}\n")

    for line in sorted_lines:
        line_errors = errors_by_line[line]
        print(f"📍 LÍNEA {line} ({len(line_errors)} conflictos):")
        print(f"{'-'*40}")
        
        for i, err in enumerate(line_errors, 1):
            ctx = extract_context(err)
            
            print(f"  [{i}] {err['type']}")
            
            if err['type'] == "Local Ambiguity":
                obs = ctx.get('observed', '??')
                look = ctx.get('lookahead', '??')
                op1 = ctx.get('option1', '(ver detalle)')
                op2 = ctx.get('option2', '(ver detalle)')
                
                print(f"      Contexto:  ... {obs}  •  {look} ...")
                print(f"      Opción A:  {op1}")
                print(f"      Opción B:  {op2}")
                
            elif err['type'] == "Conflict":
                desc = ctx.get('desc', 'Conflict details not parsed')
                print(f"      Detalle:   {desc}")
                
            elif err['type'] == "Ambiguous Grammar":
                syms = ctx.get('symbols', '??')
                print(f"      Símbolos:  {syms}")
            
            print("") 
        print("")

if __name__ == "__main__":
    main()