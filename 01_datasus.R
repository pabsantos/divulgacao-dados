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

calc_piramide_etaria <- function(uf) {
    datasus_faixa_etaria |> 
        filter(
            year(data_ocorrencia) %in% c(2019, 2020),
            nome_uf == uf
        ) |> 
        count(ano = year(data_ocorrencia), faixa_etaria, sexo_vitima) |> 
        # drop_na() |> 
        pivot_wider(
            names_from = c(sexo_vitima, ano),
            values_from = n,
            names_sep = "_",
            values_fill = 0
        ) |> 
        clean_names() |> 
        mutate(
            total_2019 = feminino_2019 + masculino_2019,
            total_2020 = feminino_2020 + masculino_2020,
            delta_masc = (masculino_2020 - masculino_2019) / masculino_2019,
            delta_fem = (feminino_2020 - feminino_2019) / feminino_2019,
            delta_total = (total_2020 - total_2019) / total_2019,
            delta_masc = scales::percent(delta_masc, accuracy = 0.1),
            delta_fem = scales::percent(delta_fem, accuracy = 0.1),
            delta_total = scales::percent(delta_total, accuracy = 0.1)
        ) |> 
        arrange(desc(faixa_etaria)) |> 
        select(
            faixa_etaria, masculino = masculino_2020,
            feminino = feminino_2020, total = total_2020,
            delta_masc, delta_fem, delta_total
        )
}
