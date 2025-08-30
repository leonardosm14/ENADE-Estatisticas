#Script para converter dados em xlsx para csv e formatar nomes das colunas.
import pandas as pd

from os import mkdir
from unicodedata import normalize
from unidecode import unidecode
from pathlib import Path

xlsx_folder = Path("raw_data/")
csv_folder = Path("csv_data/")
if not csv_folder.exists():
    mkdir(csv_folder)

for xlsx_file in xlsx_folder.iterdir():
    print(f"Converting xlsx file {xlsx_file.name} to csv and formatting.")
    df = pd.read_excel(io=xlsx_file)

    #Remove acentuações, substitui ' - ' por um único espaço, substitui espaços por '_'.
    df.columns = [
        unidecode(
            normalize("NFKD", col.replace(' - ', ' ').replace(' ', '_').lower())
        ) for col in df.columns
    ]

    df.to_csv(path_or_buf=f"{csv_folder}/{xlsx_file.name}", index=False)