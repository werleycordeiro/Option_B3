#------------------
#Scraping_Option_B3
#------------------

#url:
#----
#https://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/historico/boletins-diarios/pesquisa-por-pregao/pesquisa-por-pregao/

#selecionar:
#-----------
#"Boletim de Negociação
#BVBG.086.01 PriceReport"

#selecionar data:
#----------------
#"26/01/2022"

library(XML)
setwd("C:\\Users\\werle\\OneDrive\\Área de Trabalho\\Opções")

Dataset <- list()
datas <- c("220126","220127") # YYMMDD

for(i in 1:length(datas)){

	# Download & unzip

	url = paste0("https://www.b3.com.br/pesquisapregao/download?filelist=PR", datas[i], ".zip,")
	tmp <- tempfile(fileext = ".zip")
	utils::download.file(url = url, destfile = tmp, mode = "wb", quiet = TRUE)
	unzip(tmp, exdir = "Raw")
	setwd("Raw")
	unzip(paste0("PR",datas[i],".zip"))
	
	# Carregar os dados 

	data <- xmlParse(list.files()[1])
	xml_data <- xmlToList(data)
	data0 <- unlist(xml_data)

	# Selecionar os tickers nos dados brutos

	indices <- which(grepl("PETR", ignore.case = TRUE, data0))

	tmp <- list()

	for(i in 1:length(indices)){

		tmp_for <- data0[c(indices[i]:(indices[i]+31))] # Após achar o nome do ticker, é selecionado as 31 posições seguintes
		
		if( any(names(tmp_for)=="BizFileHdr.Xchg.BizGrp.AppHdr.BizMsgIdr") ){ # Essa string nesse if delimita o final das informações que desejamos

			check0 <- which(names(tmp_for)=="BizFileHdr.Xchg.BizGrp.AppHdr.BizMsgIdr")[1]
			tmp_for <- tmp_for[1:(check0-1)]

		}
		
		check1 <- as.numeric(tmp_for["BizFileHdr.Xchg.BizGrp.Document.PricRpt.FinInstrmId.OthrId.Id"]) > 2e+11 # Delimita o tipo de derivativo que desejamos
		
		if(check1){

			if( length(tmp_for) > 20 ){

				tmp[[i]] <- tmp_for

			}

		}
		
	}

	# O looping abaixo ordena as colunas dos dados pelo nms 
	# Primeiro é selecionado um ticker disponível, depois procura pelas informações baseado no ordenamento do nms no ticker
	# por fim, os dados são empilhados por linha e por coluna
	tmp <- tmp[lengths(tmp) != 0]
	exemplo_nomes_full <-tmp[[which(lengths(tmp)==32)[1]]]
	nms <- gsub("BizFileHdr.Xchg.BizGrp.Document.PricRpt.","",names(exemplo_nomes_full))
	nms <- gsub("FinInstrmAttrbts.","",nms)

	for(i in 1:length(tmp)){

		if( !is.null(tmp[[i]]) ){

			for( j in 1:length(nms) ){

				check0 <- any( grepl( nms[j], names(tmp[[i]]) ) )

				if(check0){

					number <- which( grepl( nms[j], names(tmp[[i]]) ) )

					if(j==1){

						final <- tmp[[i]][number]	

					}else{

						final <- c(final, tmp[[i]][number])

					}
					 
				}else{

					final <- c(final, NA)			

				}
			
			}


			if(i==1){

				final_1 <- final

			}else{

				final_1 <- rbind(final_1, final)

			}

		}
		pb = txtProgressBar(min = (1 / length(tmp) ), max = length(tmp), style = 3)
		setTxtProgressBar(pb,i)
		
	}
	colnames(final_1) <- nms	

	# Eliminar as colunas que armazenam "BRL" e "USD" e coloca como nome da coluna

	merg_names <- which(grepl("Ccy", ignore.case = TRUE, colnames(final_1)))
	new_names_ccy <- paste0( colnames(final_1)[merg_names-1],".", as.character(final_1[1,merg_names]) )
	colnames(final_1)[merg_names-1] <- new_names_ccy
	final_2 <- final_1[,-merg_names]

	remover <- c("SctyId.", "FinInstrmId.OthrId.", "FinInstrmId.OthrId.Tp.", "FinInstrmId.PlcOfListg.", "TradDtls.", ".text")
	nms2 <- colnames(final_2)

	for( i in 1:length(remover) ){

		nms2 <- gsub(remover[i],"",nms2)

	}
	colnames(final_2) <- nms2
	rownames(final_2) <- as.character(c(1:nrow(final_2)))
	Dataset[[dates[i]]] <- final_2
	#
	#head(final_2)
	#tail(final_2)
	#
	# final_2[final_2[,1]=="PETRB381",] # Selecionar ticker específico
	file.remove(list.files())

	setwd(...)
}




