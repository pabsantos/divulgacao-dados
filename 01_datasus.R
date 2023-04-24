calc_faixa_etaria <- function() {
    datasus |>
        mutate(
            faixa_etaria = cut(
                idade_vitima,
                breaks = c(
                    0, 5, 10, 15, 20, 25, 30, 35, 40,
                    45, 50, 55, 60, 65, 70, 75, 80, 100
                ),
                labels = c(
                    "0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos",
                    "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", 
                    "35 a 39 anos", "40 a 44 anos", "45 a 49 anos", 
                    "50 a 54 anos", "55 a 59 anos", "60 a 64 anos", 
                    "65 a 69 anos", "70 a 74 anos", "75 a 79 anos",
                    "Mais de 80 anos"
                ),
                include.lowest = TRUE,
                right = FALSE
            )
        )
}

arrange_faixa_etaria <- function(uf, br = FALSE) {
    
    if (br == FALSE) {
        datasus_faixa_etaria <- datasus_faixa_etaria |> filter(nome_uf == uf)
    } 
    
    datasus_faixa_etaria |> 
        filter(year(data_ocorrencia) %in% c(2019, 2020)) |> 
        count(faixa_etaria, ano = year(data_ocorrencia)) |> 
        pivot_wider(
            names_from = ano,
            values_from = n,
            names_prefix = "obitos_",
            values_fill = 0
        ) |> 
        mutate(
            variacao = (obitos_2020 - obitos_2019) / obitos_2019,
            variacao = scales::percent(variacao, accuracy = 0.1)
        ) |> 
        select(faixa_etaria, obitos_2019, obitos_2020, variacao) |> 
        arrange(desc(faixa_etaria))
}

arrange_obitos_sexo <- function(uf) {
    datasus_faixa_etaria |> 
        filter(year(data_ocorrencia) %in% c(2019, 2020)) |> 
        count(nome_regiao, nome_uf, ano = year(data_ocorrencia), sexo_vitima) |> 
        pivot_wider(
            names_from = c(sexo_vitima, ano),
            values_from = n,
            names_sep = "_",
            values_fill = 0
        ) |> 
        select(!starts_with("NA")) |> 
        arrange(nome_regiao) |> 
        drop_na() |> 
        clean_names() |> 
        mutate(
            variacao_masc = (masculino_2020 - masculino_2019) / masculino_2019,
            variacao_fem = (feminino_2020 - feminino_2019) / feminino_2019,
            variacao_masc = scales::percent(variacao_masc, accuracy = 0.1),
            variacao_fem = scales::percent(variacao_fem, accuracy = 0.1)
        )
}

arrange_piramide_etaria <- function(uf = "all") {
    if (!uf == "all") {
        datasus_faixa_etaria <- datasus_faixa_etaria |> filter(nome_uf == uf)
    }
    
    datasus_faixa_etaria |> 
        filter(year(data_ocorrencia) == 2020) |> 
        count(sexo_vitima, faixa_etaria) |> 
        drop_na() |> 
        pivot_wider(names_from = sexo_vitima, values_from = n, values_fill = 0) |> 
        arrange(desc(faixa_etaria))
}
