# Trabalho 1 – Probabilidade e Estatística (INE5405) – UFSC

Este repositório apresenta o **Trabalho 1** da disciplina de Probabilidade e Estatística (INE5405) da UFSC.  

A partir da base de dados do **Exame Nacional de Desempenho dos Estudantes (ENADE)**, o projeto realiza:

- Geração de tabelas de frequência;  
- Criação de gráficos de dispersão;  
- Cálculo de medidas de tendência central e dispersão.  

## Estrutura do Projeto:
```
.
├── data
│   ├── csv_data
│   │   ├── CPC_2021.csv
│   │   ├── CPC_2022.csv
│   │   ├── CPC_2023.csv
│   │   ├── IDD_2021.csv
│   │   ├── IDD_2022.csv
│   │   ├── IDD_2023.csv
│   │   ├── IGC_2021.csv
│   │   ├── IGC_2022.csv
│   │   └── IGC_2023.csv
│   ├── raw_data
│   │   ├── CPC_2021.xlsx
│   │   ├── CPC_2022.xlsx
│   │   ├── CPC_2023.xlsx
│   │   ├── IDD_2021.xlsx
│   │   ├── IDD_2022.xlsx
│   │   ├── IDD_2023.xlsx
│   │   ├── IGC_2021.xlsx
│   │   ├── IGC_2022.xlsx
│   │   └── IGC_2023.xlsx
│   ├── requirements.txt
│   └── xlsx_to_csv.py
├── README.md
├── relatório
│   ├── capa.tex
│   ├── figuras
│   │   └── logo-ufsc.pdf
│   ├── graficos
│   ├── main.tex
│   ├── referencias.bib
│   ├── secoes
│   │   ├── anexos-codigos.tex
│   │   ├── anexos-graficos.tex
│   │   ├── conclusoes.tex
│   │   ├── introducao.tex
│   │   ├── materiais.tex
│   │   └── resultados.tex
│   ├── tabelas
│   │   ├── conceito_enade.tex
│   │   └── tab_ciclo_trienal.tex
│   └── Trabalho 1 - INE5405.pdf
└── src
    ├── gráficos
    ├── script_geral.r
    ├── script_sc.r
    ├── tabelas
    │   ├── SC
    │   │   ├── categoria_administrativa_instituicao.csv
    │   │   ├── conceito_enade_continuo.csv
    │   │   ├── conceito_preliminar_curso_continuo.csv
    │   │   ├── contingencia_modalidade_categoria.csv
    │   │   ├── idd_continuo.csv
    │   │   ├── igc_categoria_administrativa_contingencia.csv
    │   │   ├── igc_continuo.csv
    │   │   ├── igc_continuo_universidades_cat.csv
    │   │   ├── modalidade_ensino_curso.csv
    │   │   └── tabela_medidas_resumo_sc.csv
    │   ├── tabela_frequencia_categoria_administrativa.csv
    │   └── tabela_frequencia_modalidade_ensino.csv
    └── utils.r
```
