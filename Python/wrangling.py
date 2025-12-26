"""Data wrangling script to extract binary features from property listings."""
import pandas as pd
import re
import sys

INPUT_FILE = "../Datasets/CWB_Housing.xlsx"
OUTPUT_FILE = "../Datasets/CWB_Housing_Wrangled.xlsx"

SEARCH_COLS = ['Adicionais', 'Areas_comuns', 'Areas_privativas', 'Descricao']

CHARACTERISTICS = [
    ('Party_room', [r'sal.o\s*(de\s*)?festas?', r'espa.o\s*festas?']),
    ('Game_room', [r'sal.o\s*(de\s*)?jogos?', r'sala\s*(de\s*)?jogos?']),
    ('Gym', [r'gin.stica', r'fitness', r'academia', r'muscula..o']),
    ('Pool', [r'piscina']),
    ('Sauna', [r'sauna']),
    ('BBQ', [r'churrasqueira', r'churras', r'parrilla']),
    ('Gourmet_space', [r'espa.o\s*gourmet', r'gourmet']),
    ('Sports_court', [r'quadra', r'quadras?\s*(de\s*)?(esporte|t.nis|poliesportiva|squash)']),
    ('Guardhouse', [r'guarita', r'portaria\s*24', r'port.o\s*eletr.nico']),
    ('Cameras', [r'c.mera', r'cftv', r'circuito\s*fechado', r'monitoramento', r'vigil.ncia']),
    ('Balcony', [r'varanda', r'sacada', r'terra.o', r'balc.o']),
    ('Playground', [r'playground', r'parquinho', r'brinquedoteca', r'espa.o\s*kids', r'espa.o\s*crian.as']),
]

TIPO_TO_CATEGORY = {
    'Apartamento': 'Apartamento',
    'Cobertura': 'Apartamento',
    'Studio': 'Apartamento',
    'Duplex': 'Apartamento',
    'Triplex': 'Apartamento',
    'Loft': 'Apartamento',
    'Kitnet': 'Apartamento',
    'Casa': 'Casa',
    'Sobrado': 'Casa',
    'Terreno': 'Casa',
}

APT_PATTERNS = [
    r'apto\b', r'apartamento', r'edif.cio', r'\bed\.', r'studio',
    r'loft', r'cobertura', r'kitnet', r'flat\b', r'residence\b',
    r'residencial', r'\bdorm\b', r'dormit.rio'
]

CASA_PATTERNS = [
    r'\bcasa\b', r'sobrado', r'resid.ncia\s+a\s+venda',
    r'resid.ncia\s+em\s+condom', r'condom.nio\s+fechado',
    r'village', r'resid.ncia\s+com'
]

OFF_PLAN_PATTERNS = [
    r'breve\s*lan.amento',
    r'unidades\s*dispon.veis',
    r'em\s*constru..o',
    r'na\s*planta',
    r'pr.\s*venda',
    r'lan.amento',
    r'entrega\s*prevista',
    r'previs.o\s*de\s*entrega',
]

VALID_BAIRROS = {
    'Abranches', 'Água Verde', 'Ahú', 'Alto Boqueirão', 'Alto da Glória',
    'Alto da Rua XV', 'Atuba', 'Augusta', 'Bacacheri', 'Bairro Alto',
    'Barreirinha', 'Batel', 'Bigorrilho', 'Boa Vista', 'Bom Retiro',
    'Boqueirão', 'Butiatuvinha', 'Cabral', 'Cachoeira', 'Cajuru',
    'Campina do Siqueira', 'Campo Comprido', 'Campo de Santana', 'Capão da Imbuia',
    'Capão Raso', 'Cascatinha', 'Caximba', 'Centro', 'Centro Cívico',
    'Cidade Industrial de Curitiba', 'Cristo Rei', 'Fanny', 'Fazendinha',
    'Ganchinho', 'Guabirotuba', 'Guaíra', 'Hauer', 'Hugo Lange',
    'Jardim Botânico', 'Jardim das Américas', 'Jardim Social', 'Juvevê',
    'Lamenha Pequena', 'Lindóia', 'Mercês', 'Mossunguê', 'Novo Mundo',
    'Orleans', 'Parolin', 'Pilarzinho', 'Pinheirinho', 'Portão',
    'Prado Velho', 'Rebouças', 'Riviera', 'Santa Cândida', 'Santa Felicidade',
    'Santa Quitéria', 'Santo Inácio', 'São Braz', 'São Francisco', 'São João',
    'São Lourenço', 'São Miguel', 'Seminário', 'Sítio Cercado', 'Taboão',
    'Tarumã', 'Tatuquara', 'Tingui', 'Uberaba', 'Umbará', 'Vila Izabel',
    'Vista Alegre', 'Xaxim'
}

BAIRRO_CORRECTIONS = {
    'Barigui': 'Santo Inácio',
    'Alto da Rua Xv': 'Alto da Rua XV',
    'Alto da XV': 'Alto da Rua XV',
    'Alto da Xv': 'Alto da Rua XV',
    'Alto': 'Alto da Rua XV',
    'Caiua': 'Capão Raso',
    'Champagnat': 'Bigorrilho',
    'Cidade Industrial': 'Cidade Industrial de Curitiba',
    'Cic': 'Cidade Industrial de Curitiba',
    'CIC': 'Cidade Industrial de Curitiba',
    'Itatiaia': 'Cidade Industrial de Curitiba',
    'Neoville': 'Cidade Industrial de Curitiba',
    'Ecoville': 'Mossunguê',
    'Jardim Schaffer': 'Vista Alegre',
    'Vila Lindoia': 'Lindóia',
    'Agua Verde': 'Água Verde',
    'Ahu': 'Ahú',
    'Alto Boqueirao': 'Alto Boqueirão',
    'Alto da Gloria': 'Alto da Glória',
    'Capao da Imbuia': 'Capão da Imbuia',
    'Capao Raso': 'Capão Raso',
    'Guaira': 'Guaíra',
    'Jardim Botanico': 'Jardim Botânico',
    'Jardim das Americas': 'Jardim das Américas',
    'Juveve': 'Juvevê',
    'Lindoia': 'Lindóia',
    'Merces': 'Mercês',
    'Mossungue': 'Mossunguê',
    'Portao': 'Portão',
    'Reboucas': 'Rebouças',
    'Santa Candida': 'Santa Cândida',
    'Santa Quiteria': 'Santa Quitéria',
    'Santo Inacio': 'Santo Inácio',
    'Sao Braz': 'São Braz',
    'Sao Francisco': 'São Francisco',
    'Sao Joao': 'São João',
    'Sao Lourenco': 'São Lourenço',
    'Sao Miguel': 'São Miguel',
    'Sitio Cercado': 'Sítio Cercado',
    'Taboao': 'Taboão',
    'Taruma': 'Tarumã',
    'Boqueirao': 'Boqueirão',
    'Umbara': 'Umbará',
    'Bigorrilho/': 'Bigorrilho',
    'Cetro': 'Centro',
    'Taboao Curitiba Pr': 'Taboão',
    'Cabral - Curitiba/pr': 'Cabral',
    'Parque Tangua': 'Pilarzinho',
}

STREET_PREFIXES = {'rua', 'avenida', 'alameda', 'travessa', 'praça', 'praca',
                   'rodovia', 'estrada', 'largo', 'vila', 'r.', 'av.', 'al.'}


def standardize_endereco(endereco):
    """Standardize address to 'Street, number' format."""
    if pd.isna(endereco) or not endereco:
        return None

    endereco = str(endereco).strip()

    if re.match(r'^[\d\s.,/\-]+$', endereco):
        return None

    endereco = re.sub(r'^Endereco:\s*', '', endereco, flags=re.I)
    endereco = re.sub(r'^(Rua|Avenida|Alameda|Travessa|Praça|Praca|Rodovia|Estrada):\s*', r'\1 ', endereco, flags=re.I)
    endereco = re.sub(r'\s*Apto:.*$', '', endereco, flags=re.I)
    endereco = re.sub(r'\s*Apt\.?\s*\d+.*$', '', endereco, flags=re.I)
    endereco = re.sub(r'\s*Apartamento\s*\d+.*$', '', endereco, flags=re.I)
    endereco = ' '.join(endereco.split())

    first_word = endereco.split()[0].lower() if endereco.split() else ''
    has_prefix = first_word in STREET_PREFIXES or first_word.rstrip('.') in {'r', 'av', 'al'}

    if not has_prefix and endereco:
        endereco = 'Rua ' + endereco

    if len(endereco) < 5:
        return None

    if not re.search(r'\d+', endereco):
        return None

    return endereco


def standardize_bairro(bairro):
    """Standardize bairro name to one of the 75 valid Curitiba neighborhoods."""
    if pd.isna(bairro) or not bairro:
        return None

    bairro = str(bairro).strip()

    if bairro in BAIRRO_CORRECTIONS:
        return BAIRRO_CORRECTIONS[bairro]

    if bairro in VALID_BAIRROS:
        return bairro

    bairro_lower = bairro.lower()
    for valid in VALID_BAIRROS:
        if valid.lower() == bairro_lower:
            return valid

    for wrong, correct in BAIRRO_CORRECTIONS.items():
        if wrong.lower() == bairro_lower:
            return correct

    return None


def search_characteristic(row, patterns):
    """Search for characteristic patterns in relevant columns."""
    for col in SEARCH_COLS:
        text = str(row.get(col, '')).lower()
        if not text or text == 'nan':
            continue
        for pattern in patterns:
            if re.search(pattern, text, re.IGNORECASE):
                return 1
    return 0


def detect_off_plan(row):
    """Detect if a property is off-plan based on text indicators."""
    text_fields = ['Adicionais', 'Areas_comuns', 'Areas_privativas', 'Descricao', 'Tipo']
    combined_text = ' '.join(str(row.get(col, '')) for col in text_fields).lower()

    if not combined_text or combined_text.strip() == 'nan':
        return 0

    for pattern in OFF_PLAN_PATTERNS:
        if re.search(pattern, combined_text, re.IGNORECASE):
            return 1
    return 0


def classify_property_type(row):
    """Classify property into 'Casa' or 'Apartamento'."""
    tipo = str(row.get('Tipo', '')).strip()
    categoria = str(row.get('Categoria', '')).strip()

    if tipo in TIPO_TO_CATEGORY:
        return TIPO_TO_CATEGORY[tipo]

    url = str(row.get('URL', '')).lower()
    descricao = str(row.get('Descricao', '')).lower()
    combined_text = url + ' ' + descricao

    for pattern in APT_PATTERNS:
        if re.search(pattern, combined_text, re.IGNORECASE):
            return 'Apartamento'

    for pattern in CASA_PATTERNS:
        if re.search(pattern, combined_text, re.IGNORECASE):
            return 'Casa'

    if categoria in ['Apartamento', 'Casa']:
        return categoria

    return None


def detect_outliers_iqr(series, k=1.5):
    """Detect outliers using the IQR method."""
    Q1 = series.quantile(0.25)
    Q3 = series.quantile(0.75)
    IQR = Q3 - Q1
    lower = Q1 - k * IQR
    upper = Q3 + k * IQR
    return (series < lower) | (series > upper)


def detect_outliers_zscore(series, threshold=3.0):
    """Detect outliers using z-score method."""
    mean = series.mean()
    std = series.std()
    if std == 0:
        return pd.Series(False, index=series.index)
    z_scores = (series - mean) / std
    return z_scores.abs() > threshold


def detect_price_outliers(df, min_price_m2=1000, max_price_m2=50000):
    """Detect outliers based on price per m² ratios."""
    price_per_m2 = df['Price'] / df['Usable_area_m2']
    return (price_per_m2 < min_price_m2) | (price_per_m2 > max_price_m2)


def winsorize_column(series, lower_pct=0.01, upper_pct=0.99):
    """Winsorize a series by capping values at percentiles."""
    lower = series.quantile(lower_pct)
    upper = series.quantile(upper_pct)
    return series.clip(lower=lower, upper=upper)


def create_dummies(df, col, prefix, num_dummies):
    """Create dummy variables from a numeric column (value 1 = reference)."""
    for i in range(1, num_dummies + 1):
        dummy_name = f'{prefix}{i}'
        if i == num_dummies:
            df[dummy_name] = (df[col] >= i + 1).astype(int)
        else:
            df[dummy_name] = (df[col] == i + 1).astype(int)
    return df


def main(input_file=INPUT_FILE, output_file=OUTPUT_FILE):
    print(f"Reading {input_file}...")
    df = pd.read_excel(input_file)
    print(f"Loaded {len(df)} properties with {len(df.columns)} columns")
    print()

    print("Classifying property types...")
    df['Category'] = df.apply(classify_property_type, axis=1)

    original_cats = df['Categoria'].value_counts()
    new_cats = df['Category'].value_counts()
    print(f"  Original Categoria distribution:")
    for cat, count in original_cats.items():
        print(f"    {cat}: {count}")
    print(f"  New Category distribution:")
    for cat, count in new_cats.items():
        if pd.notna(cat):
            print(f"    {cat}: {count}")

    unclassified = df['Category'].isna().sum()
    df = df.dropna(subset=['Category'])
    print(f"  Dropped {unclassified} unclassifiable properties (Terreno, etc.)")
    print(f"  Remaining: {len(df)} properties")
    print()

    print("Standardizing Bairro names...")
    df['Bairro'] = df['Bairro'].apply(standardize_bairro)
    invalid_bairros = df['Bairro'].isna().sum()
    df = df.dropna(subset=['Bairro'])
    print(f"  Dropped {invalid_bairros} rows with invalid/unmapped bairros")
    print(f"  Remaining: {len(df)} properties")
    print()

    print("Standardizing Endereco...")
    df['Endereco'] = df['Endereco'].apply(standardize_endereco)
    invalid_enderecos = df['Endereco'].isna().sum()
    df = df.dropna(subset=['Endereco'])
    print(f"  Dropped {invalid_enderecos} rows with invalid addresses")
    print(f"  Remaining: {len(df)} properties")
    print()

    column_renames = {
        'Endereco': 'Address',
        'Bairro': 'Neighborhood',
        'Area_total_m2': 'Total_area_m2',
        'Area_util_m2': 'Usable_area_m2',
        'Idade_anos': 'Age_years',
        'Preco': 'Price',
    }
    df = df.rename(columns=column_renames)

    if 'Categoria' in df.columns:
        df = df.drop(columns=['Categoria'])
    print(f"Renamed columns to English")

    print("Detecting off-plan properties from text...")
    df['Off_plan'] = df.apply(detect_off_plan, axis=1)
    off_plan_count = df['Off_plan'].sum()
    age_na_count = df['Age_years'].isna().sum()
    print(f"  Off_plan=1 (text indicators): {off_plan_count} ({100*off_plan_count/len(df):.1f}%)")
    print(f"  Age_years=NA: {age_na_count} ({100*age_na_count/len(df):.1f}%)")

    df.loc[df['Off_plan'] == 1, 'Age_years'] = 0
    print(f"  Set Age_years=0 for {off_plan_count} off-plan properties")

    missing_age_mask = (df['Off_plan'] == 0) & (df['Age_years'].isna())
    missing_age_count = missing_age_mask.sum()
    df = df[~missing_age_mask]
    print(f"  Dropped {missing_age_count} rows with Off_plan=0 and missing Age_years")
    print(f"  Remaining: {len(df)} properties")

    if 'Planta' in df.columns:
        df = df.drop(columns=['Planta'])
    print()

    print("Creating binary feature columns:")
    for col_name, patterns in CHARACTERISTICS:
        df[col_name] = df.apply(lambda row: search_characteristic(row, patterns), axis=1)
        count = df[col_name].sum()
        pct = 100 * count / len(df)
        print(f"  {col_name}: {count} ({pct:.1f}%)")

    print()

    print("Creating dummy variables:")
    df = create_dummies(df, 'N_vagas', 'PARKING', 2)
    print(f"  N_vagas -> PARKING1, PARKING2")

    df = create_dummies(df, 'N_quartos', 'BEDROOM', 4)
    print(f"  N_quartos -> BEDROOM1, BEDROOM2, BEDROOM3, BEDROOM4")

    df = create_dummies(df, 'N_banheiros', 'BATHROOM', 4)
    print(f"  N_banheiros -> BATHROOM1, BATHROOM2, BATHROOM3, BATHROOM4")

    print()

    print("Detecting outliers...")
    has_price = df['Price'].notna()

    price_log = df.loc[has_price, 'Price'].apply(lambda x: x if x <= 0 else x).pipe(
        lambda s: s[s > 0]
    )
    if len(price_log) > 0:
        price_outliers = detect_outliers_iqr(df.loc[has_price, 'Price'], k=3.0)
        print(f"  Price outliers (IQR k=3): {price_outliers.sum()}")
    else:
        price_outliers = pd.Series(False, index=df[has_price].index)

    area_outliers = (
        detect_outliers_iqr(df['Total_area_m2'], k=3.0) |
        detect_outliers_iqr(df['Usable_area_m2'], k=3.0)
    )
    print(f"  Area outliers (IQR k=3): {area_outliers.sum()}")

    impossible = (
        (df['Total_area_m2'] <= 0) |
        (df['Usable_area_m2'] <= 0) |
        (df['Usable_area_m2'] > df['Total_area_m2'] * 1.1)
    )
    print(f"  Impossible area values: {impossible.sum()}")

    price_m2_outliers = pd.Series(False, index=df.index)
    if has_price.sum() > 0:
        price_per_m2 = df.loc[has_price, 'Price'] / df.loc[has_price, 'Usable_area_m2']
        price_m2_outliers.loc[has_price] = (price_per_m2 < 500) | (price_per_m2 > 100000)
        print(f"  Price/m² outliers (<500 or >100k): {price_m2_outliers.sum()}")

    df['Outlier'] = (
        price_outliers.reindex(df.index, fill_value=False) |
        area_outliers |
        impossible |
        price_m2_outliers
    ).astype(int)

    outlier_count = df['Outlier'].sum()
    print(f"  Total flagged as outliers: {outlier_count} ({100*outlier_count/len(df):.1f}%)")
    print(f"  Outliers flagged but kept (use 'Outlier' column for sensitivity analysis)")
    print()

    legacy_cols = ['Adicionais', 'Areas_comuns', 'Areas_privativas', 'Descricao', 'IPTU', 'Tipo',
                   'N_vagas', 'N_quartos', 'N_banheiros']
    cols_to_drop = [c for c in legacy_cols if c in df.columns]
    if cols_to_drop:
        df = df.drop(columns=cols_to_drop)
        print(f"Dropped legacy columns: {cols_to_drop}")

    cols = list(df.columns)
    first_cols = ['Category', 'Address', 'Neighborhood', 'Off_plan']
    last_cols = ['Price', 'Outlier', 'URL']

    new_order = []
    for c in first_cols:
        if c in cols:
            new_order.append(c)
            cols.remove(c)

    for c in last_cols:
        if c in cols:
            cols.remove(c)

    new_order.extend(cols)
    new_order.extend([c for c in last_cols if c in df.columns])

    df = df[new_order]
    print(f"Reordered columns: {list(df.columns)}")
    print()

    print(f"Saving to {output_file}...")
    df.to_excel(output_file, index=False, engine='openpyxl')
    print(f"Done! Saved {len(df)} rows with {len(df.columns)} columns")


if __name__ == "__main__":
    input_path = sys.argv[1] if len(sys.argv) >= 2 else INPUT_FILE
    output_path = sys.argv[2] if len(sys.argv) >= 3 else OUTPUT_FILE
    main(input_path, output_path)