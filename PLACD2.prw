#include "protheus.ch"
#include "apvt100.ch"
#include "topconn.ch"
#include "rwmake.ch"
#include "tbiconn.ch"
#include "ap5mail.ch" 
#include "RPTDEF.CH"



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// @Title: Picklist de Saï¿½da - MS AMBROGIO                                                                                //                                                                   
// @Desc:                                                                                                               //                                    
// @Autor: Arinus K. - Compton                                                                                                  //                                      
// @Date: 14/03/2022                                                                                                 //   
// @Adicional: Ponto de entrada Etiqueta + Relatorio Cadsz7 + Browser Protheus CADSZ7 + Etiqueta impressao Manual;   //                                 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

User Function PLACD2()
    Local  i
    Local x
    Public aTmpNms    := {}
    Public cItemAt    := ""     //Item Atual
    Public nVolTotl   := 0.0000 //somatoria volumes
    Public nPlTotl    := 0.0000 //somatï¿½ria peso lï¿½quido
    Public nPbTotl    := 0.0000 //somatória peso bruto
    Public cResNum    := ""
    Public aDocsRes   := {}     //Array com todos os documentos de reserva picklist.
    Public cDoc       := ""
    Public aRecnz9    := {}     //todas as recnos dos registros adicionados a tabela de semaforo. Usado no momento em que o acd é encerrado
    Public cUsrss     //usuario pedido
    Public cUsrsprd  := "" //usuario produto
       
    Private nOp := 0

    Public lCntCInc := .F. //Contar caixas incompletas? (volume e qnt)
    Public lAddProd := .T. //Se o produto pode ser adicionado. Para casos em q a qnt é 0.0000 no momento de beepar, nao adiciona a linha ao Z8Itens.

/*        DbSelectArea("SZ9")
    SZ9->(DbGoTop())
    For x:=1 to 1
        RECLOCK("SZ9",.F.)
        SZ9->(DbDelete())
        SZ9->(MsUnlock())
        SZ9->(DbSkip())
    Next x */    
    

    @ 0,0 VTSAY "Selecione Uma Opcao:"
    nOp:=VTaChoice(1,0,6,VTMaxCol(),{"Incluir Picklist","Visualizar Picklist"})
/*     VTINKEY(3)
    VTALERT("TECLA:"+str(VTLASTKEY())) */
    
    
    If nOp == 1
        //Funï¿½ï¿½o Inicial do Picklist de Saï¿½da
        VTCLEAR()
        //VTSetKey(27,{|| })

        //Checa se o parametro MV_TIPRES é .T.
        //Caso contrário a reserva de estoque não considera a quantidade disponível
        //no momento.

        If GetMV("MV_TIPRES") != .T.
            VTAlert("O picklist nao pode ser iniciado devido ao parametro MV_TIPRES estar desativado.")
            VTAlert("SOLUCAO: Contate o administrador do sistema para realizar o ajuste.")
            Return
        Endif

        //Parametro referente a data de validade da reserva
        If GetMv("MV_PZRESER") != 3600
            VtAlert("Data de validade de reserva com valor padrao. Necessario alterar p/ 3600. Parametro: MV_PZRESER")
            VTAlert("SOLUCAO: Contate o administrador do sistema para realizar o ajuste.")
            Return
        Endif 

        U_INCPLS()

    Elseif nOp == 2
        U_VISUPLS() //VISUALIZAR PICKLIST
/*     Elseif nOp == 3
        U_ADMPAR()
        VTCLEAR() */
    Endif

    //Fecha todos os alias temporarios em aberto, utilizando a array aTmpNms.
    //Evita o erro Alias Aready in use
    ClsTmp()



Return

/*/{Protheus.doc} LimpaSmf(cUsr)
    *Limpa todos os registros adicionados referentes ao usuário.
    @type  Function
    @author Arinus Kruszynski de Oliveira
    @since 19/07/2022
    @version version
    @param Recebe array de recnos a serem deletados do SZ9
    @return lRet
    @see (links_or_references)
    /*/
Static Function LimpaSmf(cUsr)
    Private lRet

    //VTAlert("Limpa Smf - Usuario:"+alltrim(cUsr))
    If Select("SZ9") > 0
        SZ9->(DbCloseArea())
    Endif
    DbSelectArea("SZ9")
    DbSetOrder(3) //Filial+Usuario
    If DbSeek(xFilial("SZ9")+cUsr)


        While SZ9->(!Eof())
            If alltrim(SZ9->Z9_SOLIC) == alltrim(cUsr)
                Reclock("SZ9",.F.)
                SZ9->(DbDelete())
            Endif
            SZ9->(DbSkip())
        Enddo

        lRet := .T.
    Else
        lRet := .F.
    Endif


Return lRet

//////////////////////////////////////////////////////////////////
// Função para administração do parametro de trava de produto   //
//                                                              //
//////////////////////////////////////////////////////////////////
Static Function U_ADMPAR()
    Private cProd := SPACE(TamSx3('B1_COD')[1])
    Private bMenu := .T.
    VTCLEAR()
    While bMenu
        If !empty(GetMv("MV_PLCTRL"))
            @ 0,0 VTSAY "Insira o produto p/ exclusao."
            @ 1,0 VTSAY "Par:"+GetMv("MV_PLCTRL")
            @ 2,0 VTSAY "Prod:" VTGET cProd  VALID !empty(cProd)
            VTREAD

            If CheckDispo(cProd)
                VTALERT("O produto nao esta nos parametros de trava. Selecione outro produto.")
            Else
                If RemovPar(cProd)
                //VTALERT("Produto"+alltrim(cProd)+" removido.")
                Else
                    //VTALERT("Erro ao remover produto. Tente novamente.")
                Endif
            Endif
        else
            VTALERT("Parametro vazio. Nao existem produtos travados..")
            Return
        Endif  


            VTCLEAR()
        If !empty(GetMv("MV_PLCTRL"))        
            @ 0,0 VTSAY "Excluir mais produto s?"
            @ 1,0 VTSAY "Par:"+GetMv("MV_PLCTRL")

            nOpyWw:=VTaChoice(2,0,6,VTMaxCol(),{"1-Sim","2-Sair"})
            If nOpyww == 1
                bMenu := .T.
                cProd := SPACE(TamSx3('B1_COD')[1])
            Else
                bMenu := .F. //fim do loop while
            Endif
            VTCLEAR()
        else
            VTALERT("Parametro vazio. Nao existem produtos travados.")
            Return
        Endif
        
    Enddo

Return


/////////////////////////////////////////////
// Função Visualizar Picklist de Saída   //
///////////////////////////////////////////

Static Function U_VISUPLS()
    //Contadores
    Local q

    //Arrays
    Private aVisuVB :=  {}
    Private aItens  :=  {}
    Private aZ8It   :=  {}
    Private aItensZ8 := {}

    //Outros
    Private lExit   := .F.
    Private nOpyw   := 0


    While !lExit

        //Cabeçalho
        IF SELECT("SZ7") > 0
            SZ7->(Dbclosearea())
        ENDIF

        //Numero,Data,Hora,Solicitante,Natureza Op, Num Pedido, Nota Fiscal, Qnt Volume.
        cQuerX := "SELECT * FROM "+RetSqlName("SZ7")+" WHERE Z7_STATUS='A' AND D_E_L_E_T_='' "
        TCQUERY cQuerX NEW ALIAS "SZ7"
        TCSETFIELD("SZ7","Z7_DATA","D",8,0)

        DbSelectArea("SZ7")
        SZ7->(DbGoTop())
        
        While !SZ7->(Eof())
            Aadd(aVisuVB, {SZ7->Z7_NUM,Stod(DtoS(SZ7->Z7_DATA)),SZ7->Z7_HORA,SZ7->Z7_SOLIC,SZ7->Z7_NATOP,SZ7->Z7_PEDIDO,SZ7->Z7_NOTA,SZ7->Z7_QVOL})
            SZ7->(DbSkip())
        Enddo

        nMaxIncv := len(aVisuVB)
        aItPl := Array(nMaxIncv, 8) //cria array para montagem do menu VTABROWSE.

        For q:=1 To nMaxIncv
            aItPl[q] := {aVisuVB[q,1],aVisuVB[q,2],aVisuVb[q,3],aVisuVB[q,4],aVisuVB[q,5],aVisuVB[q,6],aVisuVB[q,7],aVisuVB[q,8]}
            Aadd(aItens,aItPl[q])
        Next

        If nMaxIncv == 0
            //Caso nao exista itens no picklist SZ7
            VTALERT("Nada retornou da consulta SQL SZ7. SOLUÇÃO: Verifique se existem picklists cadastrados.")
            Return
        Endif

        aCab := {"Num","Data            ","Hora      ","Solicitante                            ","Natureza Op   ","Num Pedido         ","Nota F.        ","Qnt Volume                  "} 
        aSize:= {TamSx3("Z7_NUM")[1],TamSx3("C6_ENTREG")[1],TAMSX3("Z7_HORA")[1],TAMSX3("Z7_SOLIC")[1],7,TamSx3("Z7_PEDIDO")[1],TamSx3("Z7_NOTA")[1],TamSx3('Z7_QVOL')[1]}  
        nPos := 1 
        nChoose  := VTaBrowse(0,0,6,31,aCab,aItens,aSize,,len(aItens))

        VTCLEAR()

        //VTAlert("Voce escolheu o picklist "+aVisuVB[nChoose,1])
        
        cPlChsd := aVisuVB[nChoose,1]

        //Puxa itens de acordo com o picklist escolhido.
        If Select("SZ8")>0
            SZ8->(DbCloseArea())
        EndIf

        //Numero,Data,Hora,Solicitante,Natureza Op, Num Pedido, Nota Fiscal, Qnt Volume.
        cQuerX := "SELECT * FROM "+RetSqlName("SZ8")+" WHERE Z8_NUM='"+cPlChsd+"' AND D_E_L_E_T_='' "
        TCQUERY cQuerX NEW ALIAS "SZ8"
        TCSETFIELD("SZ8","Z8_DATENT","D",8,0)

        DbSelectArea("SZ8")
        SZ8->(DbGoTop())

        While !SZ8->(Eof())
            aAdd(aZ8It,{SZ8->Z8_ITEM,SZ8->Z8_CODMP,SZ8->Z8_DESC,SZ8->Z8_UM,SZ8->Z8_QUANT,SZ8->Z8_LOCALIZ,SZ8->Z8_LOTE,SZ8->Z8_PESOL,SZ8->Z8_PESOB,SZ8->Z8_QCXC,SZ8->Z8_QNTINC,Stod(DtoS(SZ8->Z8_DATENT))}) 
            SZ8->(DbSkip())
        EndDo

        nMaxIt := len(aZ8It)

        aItb := Array(nMaxIt, 13) //cria array para montagem do menu VTABROWSE.

        For q:=1 To nMaxIt
            aItb[q] := {aZ8It[q,1],aZ8It[q,2],aZ8It[q,3],aZ8It[q,4],Transform(aZ8It[q,5],"@R 99999999999.9999"),aZ8It[q,6],aZ8It[q,7],Transform(aZ8It[q,8],"@E 9,999,999.999"),Transform(aZ8It[q,9],"@E 9,999,999.999"),aZ8It[q,10],Transform(aZ8It[q,11],"@E 9,999,999.999"),aZ8It[q,12]}
            Aadd(aItensZ8,aItb[q])
        Next

        //Caso o picklist estiver sem itens
        If nMaxIt != 0 
            aCab := {"Item","Produto            ","Desc         ","UM  ","Quant      ","Endereco         ","Lote        ","Peso Liq.                  ","Peso Brut.         ","Caixas Com      ","Caixas Inc.      ","Data Ent       "} 
            aSize:= {TamSx3('Z8_ITEM')[1],TamSx3('Z8_CODMP')[1],TamSx3('Z8_DESC')[1],TamSx3('Z8_UM')[1],TamSx3('Z8_QUANT')[1]+2,TamSx3('Z8_LOCALIZ')[1],TamSx3('Z8_LOTE')[1],TamSx3('Z8_PESOL')[1]+2,TamSx3('Z8_PESOB')[1]+2,TamSx3('Z8_QCXC')[1],TamSx3('Z8_QNTINC')[1]+6,8}  
            nPos := 1 
            @ 0,0 VTSAY "Itens - PL "+alltrim(cPlChsd)
            nChoose  := VTaBrowse(1,0,6,31,aCab,aItensZ8,aSize)
            VTCLEAR()
            @ 0,0 VTSAY "Opcoes:"
            nOpyw:=VTaChoice(1,0,6,VTMaxCol(),{"1-Outro Picklist","2-Sair"})
            If nOpyw == 1
                //Reset das variaveis do browser
                aItens   := {}
                aItensZ8 := {}
                aZ8It    := {}
                aItb     := {}
                aItPl    := {}
                VTCLEAR()

            Elseif nOpyw == 2
                //Sair do visualizar picklist
                VTALERT("Finalizando..","Finalizando",.T.,500)
                Return
            Endif
        else
            VTALERT("Nada retornou da consulta SQL. SOLUÇÃO: Verifique se existem itens cadastrados no picklist.")
            VTCLEAR()
        Endif



    Enddo
    

Return

////////////////////////////////////////////
// Funï¿½ï¿½o Incluir Picklist de Saï¿½da /
//////////////////////////////////////////
Static Function U_INCPLS()

    //For
    Local q := 1
    Local y := 0
    Private nQntPend := 0
    Private nPesoBli := 0 //Peso Bruto Linha (peso bruto = pesob*numcaixas)
    Private nPesoLli := 0 //Peso Liquido Linha
    Private aItx     := {}
    Private aChsedIt := {}
    Private nContlp  :=  0
    Private cItsel   := ""
    Private nOpc     :=  0
    Private cPvenda  := SPACE(6)  //preenche com espaï¿½os em branco de acordo com tamanho do campo
    Private nCntndel    := 0 //contador de itens não deletados..
    
    //Arrays:
    Private aIncVB    := {} //MONTAGEM TELA VTABROWSE
    Private aPedVenda := {} //MONTAGEM TELA VTABROWSE
    Private aItens    := {} //MONTAGEM TELA VTABROWSE
    Private aAddsys   := {} //ITENS SELECIONADOS P/ INCLUSAO DB
    Private aSugTela  := {} //MONTAGEM TELA SUGESTï¿½ES
    Private aMntSug   := {} //MONTAGEM TELA SUGESTï¿½ES
    Private aItensX   := {} //MONTAGEM TELA SUGESTï¿½ES 
    Private aReadyad  := {} //Array com os itens já adicionados
    Private aQuery    := {} //Array Query Qnt Pendente
    
    //Públicas:
    Public aZ8Itens   := {} //array para montar tela com o adicionado - CODMP,DESC,UM,ARMZ,LOTE,LOCALIZ,QNT
    Public aZ8Insrt   := {} //array final - array que serï¿½ passada a database.
    Public nZ8ItSq    := 01 //contagem sequencial dos itens no Z8.

    //Boleanas
    Private bArdyAd  := .F.
    Private bSelag   := .T.
    Private bEmpty
    Private bCanAdd  := .F. //Permitir ou não adicionar o item a tela final.
    Private bIsDispo := .F. 

    Private lPular  := .F. //Correção bug ao escolher não.
    //Outros
    Private nNatOp := "" //Natureza da Operação
    Private nTrta    := 0
    Private nTrtb    := 0
    Private nTrtc    := 0


    @ 0,0 VTSAY "Selecione a Natureza da OP:"
    nNatOp:=VTaChoice(1,0,6,VTMaxCol(),{"Venda","Beneficiamento","Devolucao","Retrabalho","Outras Saidas"})
    VTCLEAR()

    //Natureza da Op
    //Tipos:   V=Venda;B=Beneficiamento;D=Devolução;R=Retrabalho;O=OutrasSaídas;

    If nNatOp == 1    //venda
        cNatOp := "V"
    Elseif nNatOp == 2
        cNatOp := "B" //beneficiamento

        //**Inicia rotina inclusao beneficiamento**//
	//** Em desenvolvimento **
        INCBNF()
		VTAlert("Rotina em desenvolvimento. Finalizando..")
        	Return
        //****************************************//

    Elseif nNatOp == 3
        cNatOp := "D" //devolução
    Elseif nNatOp == 4
        cNatOp := "R" //retrabalho
    Elseif nNatOp == 5
        cNatOp := "O" //Outras saídas
    Endif   

    DbSelectArea("SZ5")
    DbGoBottom()
    //ADICIONA SZ7 CABEï¿½ALHO
    
    //->Numero Picklist
    cNumSoli := GETSXENUM("SZ7","Z7_NUM") 
    
    //->Data Pedido
    dDatasol := Date()
    cDatasol := DToC(dDatasol)

    //->Hora do Pedido
    tTimesol := Time()

    //->Solicitante do Pedido
    cUsrsol := FwGetUserName(RetCodUsr())

    //->Status
    cStatus := "A" //inicia status como aberto

    DbCloseArea()

    @ 0,0 VTSAY "Insira o Numero do PdV:"
    @ 2,0 VTSAY "PdV:" VTGET cPvenda  VALID ValPVenda(cPVenda)==.T.
    VTREAD
    VTCLEAR()

    //Check Pedido de Venda
/*     If  !chkPdv(cPvenda)
        //Se pedido bloqueado..
        VTAlert("O pedido "+alltrim(cPvenda)+" esta em uso pelo usuario "+alltrim(cUsrss)+". Aguarde alguns minutos e tente novamente.")
        Return
    Else
        //Se pedido disponível.. Adiciona ao semáforo 
        TravaPed(cPvenda,cUsrsol)
    Endif */
    //asdasdasd
    IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF
      
    cQry := "SELECT * FROM "+RetSqlName("SC6")+" WHERE C6_NUM='"+cPVenda+"' AND D_E_L_E_T_=''" 
    TCQUERY cQry NEW ALIAS "TMP"

    //cTst := TMP->C6_QTDVEN - TMP->C6_QTDLIB
    
    //VTALERT("Qnt Pendente:"+str(cTst))

    DbSelectArea("TMP")
    TMP->(DbGoTop())

    While !TMP->(Eof())

        //Evitar bug caso campo esteja negativo
        //Reserva PL
        If TMP->C6_XRESPL < 0
            nQtResPl := 0 //qnt reservada pelo picklist
            nTrta    := 0
        else
            nQtResPl := TMP->C6_XRESPL
            nTrta    := TMP->C6_XRESPL
        Endif

        //Reserva Liberação
        If TMP->C6_QTDEMP < 0
            nQtLibPv := 0 
            nTrtb    := 0
        Else
            nQtLibPv := TMP->C6_QTDEMP
            nTrtb    := TMP->C6_QTDEMP
        Endif

        //Qnt Faturada
        If TMP->C6_QTDENT < 0 
            nQtFatPv := TMP->C6_QTDENT //qnt faturada 
            nTrtC    := 0
        Else
            nQtFatPv := TMP->C6_QTDENT
            nTrtC    := TMP->C6_QTDENT
        Endif

        //nQntPend := TMP->C6_QTDVEN-TMP->C6_QTDENT-TMP->C6_QTDEMP-TMP->C6_XRESPL 

        nQntPend := TMP->C6_QTDVEN-nTrta-nTrtb-nTrtc
        
        //aparecer itens do pedido com qnt_pendente > 0
        If nQntPend > 0
            Aadd(aIncVB, {TMP->C6_ITEM,TMP->C6_NUM,TMP->C6_PRODUTO,TMP->C6_ENTREG,nQntPend,TMP->C6_TES})
        Endif

        TMP->(DbSkip())
    Enddo

    TMP->(dbclosearea())


    nMaxIncv := len(aIncVB) //puxa o tamanho da array para delimitar o laï¿½o For.

    aPedVend := Array(nMaxIncv, 4) //cria array para montagem do menu VTABROWSE.


    For q:=1 To nMaxIncv
        //Transform(Alltrim(aIncVB[q,5]),"@R 99999999999.9999")
        aPedVend[q] := {(aIncVb[q,1]),alltrim(aIncVB[q,2]),alltrim(aIncVB[q,3]),dToC(StoD(aIncVB[q,4])),Transform(aIncVB[q,5],"@R 99999999999.9999")}
        Aadd(aItens,aPedVend[q])
    Next

    //VTALERT("aIncvb,q,4:"+cValToChar(aIncVB[1,4]))

    If nMaxIncv >0
        VTALERT("Selecione as linhas para seguir com o processo de expedicao","Aviso",.T.,1250)
        aCab := {"It","PdV            ","Produto      ","Data Entrega                            ","Qnt Pend.   "} 
        //codmp 12,lote 10,end 30, saldo 15
        aSize:= {TamSx3("C6_ITEM")[1],TamSx3("C6_NUM")[1],TAMSX3("C6_PRODUTO")[1],TAMSX3("C6_DATFAT")[1],18}   
        nPos := 1 
        While bSelag
            lPular := .F. //reset variavel lPular
            VTCLEAR()
            aCab := {"It","PdV            ","Produto      ","Data Entrega                            ","Qnt Pend.   "} 
            //codmp 12,lote 10,end 30, saldo 15
            aSize:= {TamSx3("C6_ITEM")[1],TamSx3("C6_NUM")[1],TAMSX3("C6_PRODUTO")[1],TAMSX3("C6_DATFAT")[1],18}   
            nPos := 1 
            nChoose  := VTaBrowse(0,0,6,31,aCab,aItens,aSize)
            
            cTes := aIncVB[nChoose,6]
            //VTAlert("cTes:"+cTes)
            //cDtEnt := dToC(StoD(aIncVB[nChoose,4]))
            
            //MsgAlert("DtEnt:"+dToC(StoD(aIncVB[nChoose,4])))

            VTCLEAR()
            

            //checa se o usuario já selecionou o item do pedido de venda
            If CheckDup(StrZero(nChoose,2),aReadyad)
                cItemAt := strzero(nChoose,2)
                VTAlert("O item "+StrZero(nChoose,2)+" ja foi adicionado. Selecione um item valido.","Erro")
                bCanAdd := .F.
            Else
                cItemAt := strzero(nChoose,2)
                bCanAdd := .T.
                bIsDispo := .T.
                aAdd(aReadyad,StrZero(nChoose,2)) //Log dos itens adicionados

/*              If chkProd(aIncVB[nChoose,3],cUsrSol)
                    //Produto disponível..
                    //VTALERT("Produto "+aIncVB[nChoose,3]+" disponível. ")
                    bIsDispo := .T.
                    aAdd(aReadyad,StrZero(nChoose,2)) //Log dos itens adicionados
                Else
                    //Produto não disponível..
                    //VTAlert("cUsrSol:"+valtype(cUsrSol)+"-IncVb[nChoose,3]:"+valtype(aIncVB[nChoose,3]))
                    VTALERT("O produto "+aIncVB[nChoose,3]+" esta bloqueado temporariamente. Aguarde o usuario "+cUsrsprd+" encerrar o picklist.","Pressione Enter")
                    bIsDispo := .F.
                Endif */

                //**********************************************************************************
                //**********************************************************************************
                
            Endif





            //Realiza consulta Sql Lote x Endereço e adiciona ao menu final.
            //Caso o item for duplicado, pula para "adicionar mais itens" e nao exibe menu de adicionados.
            If bCanAdd .AND. bIsDispo

                VTCLEAR()

                //ITEM,CODMP,DESC,UM,ARMZ,LOTE,LOCALIZ,QNT   
                cCodMp := aIncVB[nChoose,3]
 
                cItemOrig  := aIncVB[nChoose,1] //qual item do pedido de venda originou a linha

                //Verifica se tabela para o produto já foi criada. Caso sim, salva a posição para uso no restante da rotina.
                //Caso nao, cria tabela e adiciona o log a aTmpNMS.
                nPosTble := ASCAN(aTmpNms,{|x| x[1]== alltrim(cCodMP) })

                //Passa nome do Alias da tabela temporaria referente ao produto
                //Alias sempre TMP_Produto
                cAliasNam := "TMP_"+alltrim(cCodMP)

                //A query real popula a tabela temporaria.
                //as atualizações posteriores são feitas em cima da temporaria, e encerradas no momento 
                //de fechamento da rotina.
                If nPosTble > 0
                    //se tabela existir, puxa nome e insere em cTableEx
                    cTableEx := aTmpNms[nPosTble,2]
                Else
                    //se tabela nao existir..
                    //cria tabela e adiciona o nome real a array
                    //aTmpNMS := {Produto,Tabela,Alias}
                    //VTALERT("A tabela nao existe.. Criando e adicionando log")
                    aAdd(aTmpNms,{alltrim(cCodMP),LxeTmp(cCodMP,cPvenda,cUsrSol),cAliasNam})
                    nPosTble := ASCAN(aTmpNms,{|x| x[1]== alltrim(cCodMP) })
                    cTableEx := aTmpNms[nPosTble,2]
                Endif

                //Puxando Desc
                DbSelectArea("SB1")
                SB1->(DbSetOrder(1)) //FILIAL+COD
                If SB1->(DbSeek(xFilial("SB1")+cCodMP))
                    //Puxa Desc
                    If !Empty(SB1->B1_DESC)
                        cDesc := SB1->B1_DESC
                    else
                        cDesc := ""
                    Endif
                    //Puxa UM
                    If !Empty(SB1->B1_UM)
                        cUm := SB1->B1_UM
                    Else
                        cUm := ""
                    Endif

                Endif
                SB1->(DbCloseArea())

                //nQntprv := val(aIncVB[nChoose,5]) //qnt prevista
                
                nQntprv := Val(Transform(aIncVB[nChoose,5],"@R 99999999999.999"))
                
                nMxEstoq := LxEMxTmp(cCodMp)                

                //Puxa sql da tela de acordo com o produto LxEMxTmp(cCodMP)
                VTALERT("Qnt prev:"+str(nQntprv)+"-MxEstoq:"+str(nMxEstoq),"Estoque")

                //Ativar contagem de caixas incompletas?
                @ 0,0 VTSAY "Consid. Caixas Inc?"
                nOpy:=VTaChoice(1,0,6,VTMaxCol(),{"Sim","Nao"})
                If nOpy == 1
                    lCntCInc := .T.
                    VTALERT("Carregando..","Aguarde",.T.,300)
                Else
                    lCntCInc := .F.
                    VTALERT("Caixas incompletas nao serao consideradas(vol,qnt)","Aviso",.T.,2000)
                Endif

                //Monta tela de acordo com o Lote x Endereço.
                If nQntprv>nMxEstoq
                    //Se o nMxEstoque estiver zerado, apenas avisa o usuario de que nao ha nenhum saldo
                    If nMxEstoq != 0
                        //nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                        VTCLEAR()
                        @ 0,0 VTSAY "Quantidade total n/ disponivel em estoque."
                        @ 1,0 VTSAY "Adicionar o total disponivel?"
                        nOp:=VTaChoice(2,0,6,VTMaxCol(),{"Sim","Nao"})
                        VTALERT("Carregando..","Aguarde",.T.,500)
                        If nOp == 1                            
                            bArdyAd := .T.

                            lPular  := .F.
                            //Puxando Desc e UM de acordo com o código de produto
                            If Select("SB1") > 0
                                SB1->(DbCloseArea())
                            Endif

                            DbSelectArea("SB1")
                            DbSetOrder(1)
                            IF SB1->(DbSeek(xFilial("SB1")+cCodMP))
                                //DESC
                                If !empty(SB1->B1_DESC)
                                    cDesc := SB1->B1_DESC
                                Else
                                    cDesc := ""
                                Endif

                                //Um
                                If !empty(SB1->B1_UM)
                                    cUm := SB1->B1_UM
                                Else
                                    cUm := ""
                                Endif

                                //Qnt Emb
                                If !empty(SB1->B1_QE)
                                    nQntEmb := SB1->B1_QE
                                Else
                                    nQntEmb := 0
                                    //VTALERT("Produto sem QntEmb cadastrada.","Aviso",.T.,1000)
                                Endif
                            Endif


                            (cAliasNam)->(DbGoTop())

                            While !(cAliasNam)->(Eof())
                                //TMP := FILIAL,LOCAL,PRODUTO,LOTE,LOCALIZ,SALDO
                                bEmpty := .T.
                                
                                //Trava para nao pegar linha com saldo == 0 
                                If (cAliasNam)->SALDO == 0
                                    (cAliasNam)->(DbSkip())
                                Else
                                    
                                    //Separando campos por var
                                    cArmz    := (cAliasNam)->LOCALX //BF_LOCAL
                                    cCodMP   := (cAliasNam)->PRODUTO //cCodMP
                                    cLote    := (cAliasNam)->LOTE //B8_LOTECTL
                                    cEnd     := (cAliasNam)->LOCALIZ //BF_LOCALIZ
                                    cSaldoli := (cAliasNam)->SALDO //B8_SALDO

                                    If BeepValid(cCodMP,cSaldoli,cLote,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                                        nNumcx  := NoRound(cSaldoli / nQntEmb , 0)   //caixas completas
                                        nCxInc  := Mod(cSaldoli,nQntEmb )           //caixas incompletas

                                        nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                                        nPesoLli := nTmpPliq * nNumcx //total peso liq
                                        
                                        //Caixa incompleta:
                                        //1- Se existir no calculo;
                                        //2- Se habilitado pelo usuario;
                                        //* Se as duas condições forem válidas, soma no volume e na quantidade.
                                        //* Se houver caixas incompletas
                                        If nCxInc > 0 .AND. lCntCInc
                                            nIncVol := 1 
                                            nPesoBli += (nTmpPbrt * nCxInc)/ nQntEmb
                                            nPesoLli += (nTmpPliq * nCxInc)/ nQntEmb
                                        Else
                                            nIncVol := 0
                                        EndIf
                                        
                                        
                                        //* lCntcInc := Se contagem caixa incompleta estiver ativado
                                        If lCntCInc
                                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,cLote,cEnd,cSaldoli,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                                        Else

                                            //* Se caixa incompleta estiver desativada, subtrai caixa inc 
                                            If Empty(nCxInc)
                                                nCxInc := 0
                                            Endif
                                            
                                            if lAddProd != .F.
                                                Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,cLote,cEnd,cSaldoli-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                                            Endif
                                        Endif
                                    
                                        nVolTotl := nVolTotl + nNumCx + nIncVol
                                        nPlTotl  := nPlTotl+nPesoLli
                                        nPbTotl  := nPbTotl+nPesoBli

                                        //Pltotal = soma peso linha * vol

                                        //Zera linha do saldo, pois toda quantidade disponivel foi 
                                        //usada para compor parte da qntprev.
                                        //Como o saldo disponivel é inferior a quantidade prevista no pedido de venda,
                                        //podemos zerar o saldo de cada linha, pois toda quantidade da linha será usada.
                                        
                                        (cAliasNam)->SALDO := 0
                                
                                    Endif
                                    (cAliasNam)->(DbSkip())
                                Endif
                                
                            Enddo
                        Else
                            lPular := .T.
                            //VTALERT("Aguarde..","Carregando",.T.,500)
                        Endif
                    Else
                        VTALERT("Nao existe saldo em estoque para atender o produto "+alltrim(cCodMP)+". O saldo em estoque é "+alltrim(str(nMxEstoq)),"Sem Estoque")
                    EndIf
                Endif

                //Lï¿½gica Adicionar Estoque Automaticamente de Acordo com o Lote x Endereï¿½o
                //1- Puxa Query Lote x End com filtro validade lote.
                //2- Pega linhas atï¿½ que o Qnt a faturar seja batido. Caso o estoque total disponï¿½vel nao seja
                //suficiente, passa todas as linhas.
                //Qntlinha = Qnt de saldo no Lote x Endereï¿½o na linha especificada.
                //Qnt prev = Qnt de saldo previsto p/ faturamento no Pedido de Venda.
                //Caso nao tenha sido adicionado na tela de sem estoque disponivel, adiciona 
                //agora.

                //MsgAlert("lPular"+cValToChar(lPular))
                If !lPular
                    //MsgAlert("lPular"+cValToChar(lPular))
                    If bArdyAd == .F.  
                        LxEDistr(cCodMP,nQntprv,cAliasNam,cItemOrig)  
                    else
                        bArdyAd := .F.
                    Endif

                    //Reset lPular
                    lPular := .F.    

                EndIf
                TelaSZ8(aZ8Itens) //monta tela com os itens desmembrados

            Endif            

            //BeepQr(aZ8Itens) //Tela para beepar itens pelo QR ou Manual.
            If !bCanAdd .OR. !bIsDispo
                @ 0,0 VTSAY "Tentar outro item?"
            Else
                @ 0,0 VTSAY "Adicionar mais itens ?"
            Endif
        
            nOpy:=VTaChoice(1,0,6,VTMaxCol(),{"Sim","Nao"})
            
            If nOpy == 1
                bSelag := .T.
                //Aadd(aAddsys, {(aIncVb[nChoose,1]),alltrim(aIncVB[nChoose,2]),alltrim(aIncVB[nChoose,3]),alltrim(aIncVB[nChoose,4]),alltrim(aIncVB[nChoose,5])}) //ADICIONA ARR ITENS Q SERAO INCLUIDOS
                if bEmpty
                    nContlp++
                Endif
                bEmpty := .F.
                nZ8ItSq++
            Elseif nOpy == 2
                bSelag := .F.

                //Tira os semaforos.. Pois ou o usuario aborta a operacao ou adiciona o picklist.
                //Nao ha como voltar a tela de distribuição.
                //LimpaSmf(cUsrsol)
                @ 0,0 VTSAY "Confirma a inclusao?"
                nOpcy:=VTaChoice(1,0,3,VTMaxCol(),{"Sim","Nao"})
                
                    if nOpcy == 1
                        If len(aZ8Itens) != 0
                            aZ8Insrt := aClone(aZ8Itens)    //aZ8Insrt copia da aZ8Itens
                            
                            //Adiciona saldo ao empenho.. A propria função checa
                            //se a linha já está empenhada.
                            //Empenhou altera aZ8Insrt linha 15 para .T.
                            //Por padrão a linha 15 começa como .F..
                            //VTALERT("a1")
                            //Incluindo Cabeï¿½alho
                            RECLOCK("SZ7",.T.) ///// .T. QDO INCLUSÃO, .F. QNDO ALTERAÇÃO
                            REPLACE SZ7->Z7_FILIAL WITH FWFilial()
                            REPLACE SZ7->Z7_NUM WITH cNumsoli
                            REPLACE SZ7->Z7_DATA WITH Date()
                            REPLACE SZ7->Z7_SOLIC WITH cUsrsol
                            REPLACE SZ7->Z7_HORA WITH Time()
                            REPLACE SZ7->Z7_STATUS WITH cStatus
                            REPLACE SZ7->Z7_PEDIDO WITH aIncVB[1,2] //1 pq a variavel ï¿½ preenchida com smnt 1 pdv
                            REPLACE SZ7->Z7_QVOL WITH nVolTotl
                            REPLACE SZ7->Z7_PESOL WITH nPlTotl
                            REPLACE SZ7->Z7_PESOB WITH nPbTotl 
                            REPLACE SZ7->Z7_NATOP WITH cNatop

                            SZ7->(MSUNLOCK())

                            //ADICIONA SZ8 ITENS
                            nMxItens := len(aZ8Insrt) //pega total de itens selecionados
                            //VTALERT("a2")
                            //z8itens{codmp,desc,um,arm,lote,end,qnt}
                            //1 cCodMp,2 cDesc,3 cUm,4 cArmz 05,TMP->B8_LOTECTL 06,TMP->BF_LOCALIZ 07,TMP->B8_SALDO 08,nTmpPliq 09,nTmpPbrt10,nNumCx 11,nCxInc 12
                            nCot := 1
                            For q:= 1 To nMxItens
                                //Se nao houver caixas completas e houver caixa incompleta e Considerar Inc == .F.
                                
                                //AdResLin -> Tenta adicionar reserva a partir das informações contidas na linha do Pl
                                //Caso nao consiga reservar, pode indicar indisponibilidade de estoque
                                    //VTALERT("a3")
                                    
                                    If AdResLin(aZ8Insrt[q,1],aZ8Insrt[q,5],aZ8Insrt[q,6],aZ8Insrt[q,7],cNumSoli,aZ8Insrt[q,12],cPvenda,cUsrsol,StrZero(q,2),"V")
                             
                                        RECLOCK("SZ8",.T.) ///// .T. QDO ï¿½ INCLUSï¿½O, .F. QDO ï¿½ ALTERAï¿½ï¿½O
                                        REPLACE SZ8->Z8_FILIAL WITH FWFilial()
                                        REPLACE SZ8->Z8_NUM WITH cNumsoli
                                        REPLACE SZ8->Z8_ITEM WITH StrZero(nCot,2)
                                        //vtalert("strzero:"+StrZero(q,2))
                                        REPLACE SZ8->Z8_CODMP WITH Upper(aZ8Insrt[q,1])
                                        REPLACE SZ8->Z8_DESC WITH aZ8Insrt[q,2]
                                        REPLACE SZ8->Z8_UM WITH aZ8Insrt[q,3]
                                        REPLACE SZ8->Z8_ARMZ WITH aZ8Insrt[q,4]
                                        REPLACE SZ8->Z8_QUANT WITH aZ8Insrt[q,7]
                                        REPLACE SZ8->Z8_LOTE WITH Upper(aZ8Insrt[q,5])
                                        REPLACE SZ8->Z8_LOCALIZ WITH Upper(aZ8Insrt[q,6])
                                        REPLACE SZ8->Z8_PESOL WITH aZ8Insrt[q,8]
                                        REPLACE SZ8->Z8_PESOB WITH aZ8Insrt[q,9]
                                        REPLACE SZ8->Z8_QCXC WITH aZ8Insrt[q,10]
                                        REPLACE SZ8->Z8_QNTINC WITH aZ8Insrt[q,11]
                                        REPLACE SZ8->Z8_ITPDV WITH aZ8Insrt[q,12]
                                        REPLACE SZ8->Z8_DATENT WITH STod(aZ8Insrt[q,13])
                                        REPLACE SZ8->Z8_TES WITH aZ8Insrt[q,16]
                                    
                                        //Salva qual a escolha do usuário no input da linha ACD.
                                        If  aZ8Insrt[q,14]
                                            REPLACE SZ8->Z8_OPINC WITH "Sim"
                                        Else
                                            REPLACE SZ8->Z8_OPINC WITH "Nao"
                                        Endif
                                    
                                        //Flag se a linha foi empenhada ou não.
                                        REPLACE SZ8->Z8_QEMP WITH aZ8Insrt[q,7]

                                        REPLACE SZ8->Z8_DOCRES with cDoc
                                        
                                        //1 cCodMp,2 cDesc,3 cUm,4cArmz,5 TMP->B8_LOTECTL,6 TMP->BF_LOCALIZ,7 TMP->B8_SALDO,8 nTmpPliq,nTmpPbrt,nNumCx,nCxInc
                                        SZ8->(MSUNLOCK())
                                   
                                        nCot++
                                    else
                                        //Conta quantos itens não foram reservados.
                                        //Se todos os itens nao foram reservados, não adiciona o Picklist.
                                        nCntndel++

                                        VTALERT("Erro ao reservar item "+StrZero(q,2)+". O item nao sera adicionado ao PL.")
                                    Endif

                            Next


                            If nCntnDel == nMxItens
                                //Se nenhum item puder ser adicionado  
                                RollBackSx8()
                                VTAlert("Nenhum item pode ser adicionado devido a indisponibilidade de saldo. Abortando inclusão picklist..","Pressione Enter")
                                VTCLEAR()
                                Return
                            Else
                                ConfirmSx8()
                            EndIf
                            
                            Endif

                            VTCLEAR()
                            //Adiciona empenho nos produtos utilizados no picklist.
                            ConfirmSx8()
                            VTALERT("PL Saida num "+cNumSoli+" adicionado.","Sucesso",.T.,4000)
                            //Apï¿½s adicionar, envia a notificaï¿½ï¿½o automï¿½tica via e-mail
                            cData := DtoC(Date()) //data
                            cHora := Time() //hora
                            VTALERT("Enviando Notificacao Automatica via E-mail..","Aguarde",.T.,4000)
                            AutoEml(cNumsoli,cData,cHora,cUsrsol,aIncVB[1,2],aZ8Insrt,cStatus)
                            //Obs: as tabelas temporarias sao encerradas na função principal.

                        Else
                            RollBackSx8()
                            VTALERT("O picklist nao pode ser adicionado. MOTIVO: Nenhum item adicionado.")
                        Endif

                    Elseif nOpcy ==2
                        RollbackSx8()
                        VTALERT("Abortado pelo usuario. Inclusao nao realizada","Finalizando",.T.,4000)
                        Return
                    Endif


            
            
        Enddo
        
        @ 0,0 VTSAY "S:"

        VTCLEAR()
    else
        VTALERT("Nenhum registro encontrado com o numero "+cPVenda,"Pedido De Venda Invalido/Vazio",.T.,2000)
        VTCLEAR()
        //Limpa semaforo usuario
        //LimpaSmf(RetCodUsr())
        RollBackSx8()
        //LimpaSmf(cUsrsol)

    Endif


Return




/*/{Protheus.doc} U_INCBNF
    Inclui picklist do tipo beneficiamento.
    A inclusão acontece via beep qrcode + dados preenchidos manualmente.
    Apos inclusao os pedidos de venda são criados automaticamente. Caso a opcao
    remessa seja escolhida, são criados 2 pedidos: 1 com os produtos e outro com as embalagens dos respectivos produtos.
    @type  User Function
    @author Arinus K. de Oliveira
    @since 26/07/2022
    /*/
Static Function INCBNF()
    Private q
    //Arrays
    Private aPvRem := {} //Array com todos campos que serão adicionados no pedido de venda embalagem
    Private aZ8Itens  := {} //Item,Prod,Desc,U.M,Armazem,Localização,Lote,Qnt,PesolLin,PesobLin,Qnt Caixas,Qnt Caixa Inc,Data de Entrega,Item Pdv,OpcUsucx,Qnt Res, Doc Res, Cod For, Cod Trans, Op Usr Rem
    //Utils
    Private lMnMenu  := .T.
    //Codigo Fornecedor
    Private cCodF    := SPACE(TamSx3('A2_COD')[1])   //codigo de fornecedor..
    Private cBqrc    := SPACE(45)
    //Opcao Usuario..
    Private nOpcy    := 0
    //Array QrCode
    Private aBeef     := {}
    //Campos Commit
    Private cNatOp    := "B" //Natureza Operacao
    //Itens PL
    Private nItPl     := 1
    Private aZ8Itens  := {} //Item,Prod,Desc,U.M,Armazem,Localização,Lote,Qnt,PesolLin,PesobLin,Qnt Caixas,Qnt Caixa Inc,Data de Entrega,Item Pdv,OpcUsucx,Qnt Res, Doc Res, Cod For, Cod Trans, Op Usr Rem
    //Vars Pedido
/*     Private _aItemPV := {} //Itens Pedido
    Private _aCabPV  := {} //Cabeçalho Pedido */
    //Vars PL
    Private cProd     := "" //Produto
    Private cDesc     := "" //Descrição
    Private cUm       := "" //U.M
    Private cLocal    := "" //Armazem
    Private cEnd      := SPACE(TamSx3('BF_LOCALIZ')[1]) //Endereço
    Private cLote     := "" //Lote
    Private nQtde     := 0  //Quantidade
    Private nPesoll   := 0.000  //Peso Liquido Linha Numerico
    Private nPesobl   := 0.000 //Peso Bruto Linha Numerico
    Private nNumCx    := 0  //Quantidade de Caixas
    Private nNumCxX   := 0 //Numero de caixas quando prod/lote/end repetido
    
    Private cCodEmb   := SPACE(10)
    Private nCxLinha  := 0 //Calculo Manual Qnt Caixas - Qnt Caixas Linha
    Private nCxTotal  := 0 //Calculo Manual Qnt Caixas - Qnt Caixas Cabeçalho Pl
    Private nVolLinha := 0 //Volume Linha PL
    Private nVolTotal := 0 //Volume total PL - Cabeçalho

    Private nCxInc    := 0.0000  //Caixa Incompleta (qnt) se houver
    Private cDatEnt   := ""
    Private cCodTrans := ""
    Private nRepEm    := 0
    Private cTes      := "" //Tipo Entrada/Saída
    Private nQtCxs    := 0 //Qnt de Caixas.. Informado manualmente pelo usuário.
    Private cTipoEmb  := SPACE(10)
    //Logicos
    Private lBpinv   := .F.
    //Cabeçalho
    Private nVolCab := 0
    Private nPlCab  := 0
    Private nPbCab  := 0

    //Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00})
    //Produto,Desc,Um,Armazem,Lote,Endereço,QtdLinha,PesoLiqLine,PesoBrutoLdine,Caixas,Qtd Caixa Inc (se existir),DataEntrega,etc
    
    While lMnMenu
        lBpInv := .F. //reset
        aBeef  := {}

        //1- Beep
        //2- Se beep valido, continua para adicionar mais itens.
        //3- Adicionar mais itens?
        //4- Confirmar picklist?

        @ 0,0 VTSAY "Insira o codigo de fornecedor:"
        @ 2,0 VTSAY "CF:" VTGET cCodf VALID ValFornec(cCodf)
        VTREAD
        VTCLEAR()

        //Puxa Código Transportadora
        If Select("SA2") > 0
            SA2->(DbCloseArea())
        Endif

        DbSelectArea("SA2")
        DbSetOrder(1)
        If SA2->(DbSeek(xFilial("SA2")+cCodF+""))
            cCodTrans := SA2->A2_TRANSP
        Else
            VtAlert("Codigo de Transportadora nao encontrado.")    
        Endif

        VTALERT("Para adicionar itens realize o beep do QrCode correspondente.","Pressione Enter")

        aBeef := BeepBnf()

        If aBeef != NiL
            //aBeef := Produto,Qtde,Lote e Peso Bruto.
            cProd  := PADR(aBeef[1,1],TamSx3('B1_COD')[1])
            nQtde  := aBeef[1,2]
            cLote  := aBeef[1,3]
            cLocal := "01" 
            cPbrto := ""
            
            //Qnt de Caixas
            @ 0,0 VTSAY "Insira a Qnt de Caixas:"
            @ 2,0 VTSAY "Cxs:" VTGET nQtCxs VALID !empty(nQtCxs)
            VTREAD
            VTCLEAR()

            //Endereço
            @ 0,0 VTSAY "Insira o endereco"
            @ 2,0 VTSAY "End:" VTGET cEnd VALID ValEnd(cEnd) 
            VTREAD
            VTCLEAR()
                      
            //Checa se o produto já foi adicionado no lote e endereço especificado
            //nPosB := ASCAN(aZ8Itens,{|x| x[1]==cProd .AND. x[3]==cUm .AND. x[5]==cLote .AND. x[6]==cEnd})
            
            If Select("SB1") > 0
                SB1->(DbCloseArea())
            EndIf

            DbSelectArea("SB1")
            DbSetOrder(1) //Filial+Cod
            If !DbSeek(xFilial("SB1")+cProd)
                VTAlert("Inconsistencia produto. Finalizando..")
                Return
            Endif

            //Descrição
            If !empty(SB1->B1_DESC)
                cDesc := SB1->B1_DESC
            Else
                cDesc := ""
            Endif

            //U.M
            If !empty(SB1->B1_UM)
                cUm := SB1->B1_UM
            Else
                cUm := ""    
            Endif

            //Qnt Por Embalagem
/*             If !empty(SB1->B1_QE)
                nQtEmb := SB1->B1_QE
            else
                nQtEmb := 0
                VTAlert("Produto sem quantidade por embalagem cadastrada.")
            Endif */

            If alltrim(cUm) == "MIL"
                //Se MiL-> Qnt * Peso L Pç
                //VTAlert("MIL")
                nPesoll := SB1->B1_PESO*nQtde//Peso Liquido Peça*Qtde / Prod,qnt,lote
            Elseif alltrim(cUm) == "KG"
                //VTAlert("KG")
                //Se Kg
                nPesoll := nQtde            //Para KG, PesoLiquido := Quantidade
                //VTAlert("nPesoll:"+str(nPesoll))
            EndIf

            //Peso Bruto Manual
            @ 0,0 VTSAY "Insira o Peso Bruto:"
            @ 2,0 VTSAY "PB:" VTGET nPesobl
            VTREAD
            VTCLEAR() 
            
            //Remessa para embalagem:
            @ 0,0 VTSAY "Remessa para Embalagem?"
            nOprem:=VTaChoice(1,0,3,VTMaxCol(),{"Sim","Nao"})
            VTREAD
            VTCLEAR() 

            If nOpRem == 1
                cCodEmb :=  SPACE(10)
                @ 0,0 VTSAY "Insira o Código de Embalagem:"
                @ 2,0 VTSAY "Emb:" VTGET cCodEmb VALID !empty(cCodEmb)
                VTCLEAR()
            Endif


            //Calculo Qnt de Caixas Antigo de Acordo com Qtd Embalagem - SZ8
            //nNumCx  := NoRound(nQtde / nQtEmb , 0)
            
            //**AGLUTINA**
            //Se já existir produto+lote+endereço+um na array insere na mesma posição e aglutina
            //somente o campo quantidade.
            nPosB := ASCAN(aZ8Itens,{|x| x[1]==cProd .AND. x[3]==cUm .AND. x[5]==cLote .AND. x[6]==cEnd})

            If nPosB != 0
                //**Encontrado Prod/Um/Lote/Endereço**

                aZ8Itens[nPosB,7]  := aZ8Itens[nPosB,7]  + nQtde      //Soma Quantidade
                aZ8Itens[nPosB,8]  := nPesoll   //Soma Peso Liquido Linha
                aZ8Itens[nPosB,9]  := nPesobl  //Soma Peso Bruto Linha
                aZ8Itens[nPosB,10] := aZ8Itens[nPosB,10] + nQtCxs  //Soma Numero de Caixas..

                //Calculo Qnt Caixas/Volume Novo
                nNumCxx := nQtCxs 

                nPlTotl += nPesoll * nQtCxs //Soma Peso Líquido Cabeçalho
                nPbTotl += nPesobl * nQtCxs //Soma Peso Bruto Cabeçalho
                nVlTotl += nQtCxs           //Soma Volume Cabeçalho


                //Calculo Qnt de Caixas Antigo - SZ8
                //nNumCxX  := NoRound(nQtde/ nQtEmb , 0)
                //Soma no cabeçalho Peso Líquido e Peso Bruto Linha
                //nPlTotl  += nPesoll * nNumCxX
                //nPbTotl  += nPesobl * nNumCxX
                //Soma Volume
                //nVolTotl += nNumCxx
                //*****************************************

                //VTAlert("nPlCab:"+str(nPlCab)+"-nPbCab:"+str(nPbCab)+"-nNumCx:"+str(nNumCxX))
         
                VTAlert("A quantidade sera aglutinada a linha ja existente no PL. (Prod/Um/Lote/End ja existentes).","Pressione Enter")
            Else
                //**nao encontrado**
                
                //Calculo Qnt Caixas/Volume Novo 
                nNumCxx  := nQtCxs
                nPlTotl  += nPesoll * nNumCxx
                nPbTotl  += nPesobl * nNumCxx
                nVolTotl += nNumCxx

                //**Calculo Qnt de Caixas Antigo - SZ8
                //nNumCxX  := NoRound(nQtde / nQtEmb , 0)
                //Soma no cabeçalho Peso Líquido e Peso Bruto
                //nPlTotl  += nPesoll * nNumCxX
                //nPbTotl  += nPesobl * nNumCxX
                //Soma Volume
                //nVolTotl += nNumCxx


                aAdd(aZ8Itens,{cProd,cDesc,cUm,cLocal,cLote,cEnd,nQtde,nPesoll,nPesobl,nNumCx,nCxInc,"",cDatEnt,.F.,0.00,cTes,cCodEmb})
        
            EndIf

            //**FIM AGLUTINA**

            //Itera Contador Item
            nItPl    := nItPl++   

        Else
            //Se beep for invalido..
            lBpInv := .T.
        Endif

            //Monta tela com os itens adicionados..
            //1-aPrep[q] := {strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,2],aZ8Itens[q,3],aZ8Itens[q,4],aZ8Itens[q,5],aZ8Itens[q,6],Transform(aZ8Itens[q,7],"@R 99999999999.9999"),Transform(aZ8Itens[q,8],"@R 99999999999.999"),Transform(aZ8Itens[q,9],"@R 99999999999.999")}
            //Item,prod,
            TelaSZ8(aZ8Itens)

        //Adicionar mais itens?
        @ 0,0 VTSAY "Adicionar mais itens?"
        nOpcz:=VTaChoice(1,0,3,VTMaxCol(),{"Sim","Nao"})
        If nOpcz == 1
            lMnMenu := .T.

            //Reseta variaveis..
            cBqrc    := SPACE(45)                        //QrCode
            nPesobl  := 0                               //Peso Bruto Linha
            cEnd     := SPACE(TamSx3('BF_LOCALIZ')[1]) //Endereço

        Elseif nOpcz == 2
            lMnMenu := .F.
        Endif

    EndDo

    //Inclusão Picklist + Criação dos Pedidos de Venda
    //Adicionar mais itens?
    VTCLEAR()
    @ 0,0 VTSAY "Confirma inclusao?"
    nOpcp:=VTaChoice(1,0,3,VTMaxCol(),{"Sim","Nao"})
    If nOpcp == 1
        
        //Se nenhum item foi adicionado..
        if len(aZ8Itens) == 0
            VTAlert("O picklist nao sera adicionado. MOTIVO: Nenhum item adicionado.")
            Return
        Endif
        
        //Se remessa, cria 2 pedidos de venda, 1 para os produtos e outro para emb

        If nOprem == 1
            //Opcao 1 - Criação de 1 pedido para os produtos e 1 pedido para as embalagens.
            //Criar Pedido de Remessa no Início
            CriaPdv(aZ8Itens,"R",cCodF,nOpRem)

        Elseif nOprem == 2
            //Cria 1 pedido de venda para os produtos somente.
            cDc := CriaPdv(aZ8Itens,"N",cCodf,nOpRem)

            If !empty(cDc) //se função retornar o numero do documento..
                //**Inclusão do Picklist**
                aZ8Insrt := AClone(aZ8Itens)
                cNumSoli := GetSxeNum("SZ7", "Z7_NUM")
                cUsrsol := FwGetUserName(RetCodUsr())
                cData := DtoC(Date()) //data
                cHora := Time() //hora

                SZ7->(DbGoBottom())
                SZ8->(DbGoBottom())

                RECLOCK("SZ7",.T.) ///// .T. QDO INCLUSÃO, .F. QNDO ALTERAÇÃO
                REPLACE SZ7->Z7_FILIAL WITH FWFilial()
                REPLACE SZ7->Z7_NUM WITH cNumsoli
                REPLACE SZ7->Z7_DATA WITH Date()
                REPLACE SZ7->Z7_SOLIC WITH cUsrsol
                REPLACE SZ7->Z7_HORA WITH Time()
                REPLACE SZ7->Z7_STATUS WITH "A"
                REPLACE SZ7->Z7_PEDIDO WITH cDc 
                REPLACE SZ7->Z7_QVOL WITH nVolTotl
                REPLACE SZ7->Z7_PESOL WITH nPlTotl
                REPLACE SZ7->Z7_PESOB WITH nPbTotl
                REPLACE SZ7->Z7_NATOP WITH "B"
                SZ7->(MSUNLOCK())

                //1-Prod,2-Desc,3-Um,4-Local,5-Lote,6-Endereço,7-Qnt,8-PesoLiquido,9-Peso Bruto,10-Num Caixas,11-Caixa inc,12-X,13-Data Entrega
                For q:=1 To Len(aZ8Itens)
                    //1-Prod,2-Desc,3-Um,4-Local,5-Lote,6-Endereço,7-Qnt,8-PesoLiquido,9-Peso Bruto,10-Num Caixas,11-Caixa inc,12-X,13-Data Entrega
                    
                    //1-Produto,2-Lote,3-Endereço,4-Qnt,5-Num PL,6-Item Pdv,7-Pedido,Usuario,8-Item Picklist
                    If AdResLin(aZ8Insrt[q,1],aZ8Insrt[q,5],aZ8Insrt[q,6],aZ8Insrt[q,7],cNumSoli,aZ8Insrt[q,12],cPvenda,cUsrsol,StrZero(q,2),"B")
                        //Se posição 
                        RECLOCK("SZ8",.T.) 
                        REPLACE SZ8->Z8_FILIAL WITH FWFilial()
                        REPLACE SZ8->Z8_NUM WITH cNumsoli
                        REPLACE SZ8->Z8_ITEM WITH StrZero(q,2)
                        REPLACE SZ8->Z8_CODMP WITH Upper(aZ8Insrt[q,1])
                        REPLACE SZ8->Z8_DESC WITH aZ8Insrt[q,2]
                        REPLACE SZ8->Z8_UM WITH aZ8Insrt[q,3]
                        REPLACE SZ8->Z8_ARMZ WITH aZ8Insrt[q,4]
                        REPLACE SZ8->Z8_QUANT WITH aZ8Insrt[q,7]
                        REPLACE SZ8->Z8_LOTE WITH Upper(aZ8Insrt[q,5])
                        REPLACE SZ8->Z8_LOCALIZ WITH Upper(aZ8Insrt[q,6])
                        REPLACE SZ8->Z8_PESOL WITH aZ8Insrt[q,8]
                        REPLACE SZ8->Z8_PESOB WITH aZ8Insrt[q,9]
                        REPLACE SZ8->Z8_QCXC WITH aZ8Insrt[q,10]
                        REPLACE SZ8->Z8_QNTINC WITH aZ8Insrt[q,11]
                        REPLACE SZ8->Z8_ITPDV WITH aZ8Insrt[q,12]
                        REPLACE SZ8->Z8_DATENT WITH STod(aZ8Insrt[q,13])
                        REPLACE SZ8->Z8_DOCRES WITH cDoc
                        
                        SZ8->(MsUnlock())
                    Else
                        VTALERT("Erro ao reservar item "+StrZero(q,2)+". O item nao sera adicionado ao PL.")
                    Endif
                    
                Next q

                ConfirmSx8()
                
                VTAlert("Picklist n-"+alltrim(cNumSoli)+" e Pedido de Venda n-"+alltrim(cDoc)+" adicionados.","Sucesso",.T.,3000)

                //**E-mail automático           
                AutoEml(cNumsoli,cData,cHora,cUsrsol,cDoc,aZ8Insrt,"A")
                //VTAlert("Notificacao via e-mail enviada: [ACD]Notificacao Automatica-Picklist de Saida N-"+alltrim(cNumSoli)+"-Hora:"+cHora)
                VTALERT("Enviando Notificacao Automatica via E-mail..","Aguarde...")
            Else
                VTAlert("O picklist nao sera adicionado MOTIVO: Erro ao incluir pedido(s) de venda. ")    
            Endif
            
        Endif

    Elseif nOpcp == 2
        //Aborta inclusão picklist
        VTAlert("Abortado pelo usuario. Inclusao nao realizada.","Finalizando...",.T.,4000)
        Return
    Endif

Return


/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   @@@@@@@@ Funï¿½ï¿½es utilitï¿½rias: @@@@@@@
   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */



////////////////////////////////////////////////////////////////////////////
//Função CriaPdv                                                                            //
//Inclui pedido de venda com base nos itens beepados/adicionados do PL de               //
//beneficiamento                                                                        //
//Par: Array Grid Itens, Tipo Pedido (N-Normal,R-Remessa)                               //
//Ret: Numero do pedido de venda.                                                       //
//Pars: array itens z8, tipo pedido de venda e cliente/fornecedor,Escolha do usuário    //
/////////////////////////////////////////////////////////////////////////////////////////

//Logica 1 - Produto + Embalagem.
// * Se nOpc for 1, cria pedido venda e remessa generico e depois adiciona o necessário
//

Static Function CriaPdv(aZ8Itens,cTipoPd,cCodf,nOpRem)
    Local nLinha    := 1
    Local aCabec    := {}
    Local aItens    := {}
    Local aErroAuto := {}
    Local aItens    := {}
    Local nCount    := {}
    Private nPrcVen := 1
    Private lRt 

    Private lMsErroAuto    := .F.
    Private lAutoErrNoFile := .F.

    cDoc := GetSxeNum("SC5", "C5_NUM")

    //Criação de 1 Pedido Normal - Apenas Produtos
    
    If cTipoPd == "N"
        //****************************************************************
        //* Inclusao - INÍCIO                                            *
        //****************************************************************
        aCabec := {}
        aItens := {}
        aLinha := {}
        aAdd(aCabec, {"C5_NUM",     cDoc,        Nil})
        aAdd(aCabec, {"C5_TIPO",     "B",        Nil})
        aAdd(aCabec, {"C5_CLIENTE",  cCodf,   Nil})
        aAdd(aCabec, {"C5_LOJACLI",     "01",    Nil})
        aAdd(aCabec, {"C5_LOJAENT",     "01",    Nil})
        aAdd(aCabec, {"C5_CONDPAG",     "001",    Nil})
        aAdd(aCabec, {"C5_NATUREZ",     "",      NiL})
        
        For nLinha := 1 To len(aZ8Itens)
        //aAdd(aZ8Itens,{StrZero(nItPl,2),cProd,cDesc,cUm,cLocal,cEnd,cLote,nQtde,cPesoll,cPesobl,nNumCx,nCxInc,"",cDatEnt,.F.,0.00})
        //1-Prod,2-Desc,3-Um,4-Local,5-Lote,6-Endereço,7-Qnt,8-PesoLiquido,9-Peso Bruto,10-Num Caixas,11-Caixa inc,12-X,13-Data Entrega
            aLinha := {}
            aadd(aLinha,{"C6_ITEM",    StrZero(nLinha,2), Nil})
            
            aadd(aLinha,{"C6_PRODUTO", aZ8Itens[nLinha,1],        Nil})

            aAdd(aLinha,{"C6_UM", aZ8Itens[nLinha,3],NiL})

            aAdd(aLinha,{"C6_DESC",aZ8Itens[nLinha,2],NiL})

            aadd(aLinha,{"C6_QTDVEN", aZ8Itens[nLinha,7] , Nil })

            aadd(aLinha,{"C6_PRCVEN",  nPrcVen,          Nil})

            aadd(aLinha,{"C6_PRUNIT",  0,          Nil})

            aadd(aLinha,{"C6_VALOR",   nPrcVen*aZ8Itens[nLinha,7],  Nil})    //Preço Unitário * Quantidade

            aadd(aLinha,{"C6_QTDLIB" , 0 , NiL})

            //aadd(aLinha,{"C6_LOTECTL", aZ8Itens[nLinha,5],   NiL})
            //aadd(aLinha,{"C6_LOCALIZ", aZ8Itens[nLinha,6], NiL })

            aadd(aLinha,{"C6_TES",     "501",        Nil})

            aadd(aItens, aLinha)

        Next nLinha

        nOpcX := 3 

        MSExecAuto({|a, b, c, d| MATA410(a, b, c, d)}, aCabec, aItens, nOpcX, .F.)

        If lMsErroAuto
            VTAlert("Erro na inclusao do pedido de venda.[EXECAUTO]")
            MostraErro("\system\",cDoc+".log")
            RollBackSx8()

            cDoc  := ""
        Else
            ConfirmSx8()
            //VTAlert("Pedido de Venda n-"+alltrim(cDoc)+" adicionado.","Sucesso",.T.,3000)
        Endif
    
    //Criaçao de 1 Pedido Produtos + 1 Pedido Embalagens
    Elseif cTipoPd == "R"
        //****************************************************************
        //* Inclusao - INÍCIO                                            *
        //****************************************************************
        aCabec := {}
        aItens := {}
        aLinha := {}
        aAdd(aCabec, {"C5_NUM",     cDoc,        Nil})
        aAdd(aCabec, {"C5_TIPO",     "R",        Nil})
        aAdd(aCabec, {"C5_CLIENTE",  cCodF,   Nil})
        aAdd(aCabec, {"C5_LOJACLI",     "01",    Nil})
        aAdd(aCabec, {"C5_LOJAENT",     "01",    Nil})
        aAdd(aCabec, {"C5_CONDPAG",     "001",    Nil})
        
        For nLinha := 1 To len(aZ8Itens)
        //aAdd(aZ8Itens,{StrZero(nItPl,2),cProd,cDesc,cUm,cLocal,cEnd,cLote,nQtde,cPesoll,cPesobl,nNumCx,nCxInc,"",cDatEnt,.F.,0.00})
        //1-Prod,2-Desc,3-Um,4-Local,5-Lote,6-Endereço,7-Qnt,8-PesoLiquido,9-Peso Bruto,10-Num Caixas,11-Caixa inc,12-X,13-Data Entrega
            aLinha := {}
            aadd(aLinha,{"C6_ITEM",    StrZero(nLinha,2), Nil})
            
            aadd(aLinha,{"C6_PRODUTO", aZ8Itens[nLinha,1],        Nil})

            aAdd(aLinha,{"C6_UM", aZ8Itens[nLinha,3],NiL})

            aAdd(aLinha,{"C6_DESC",aZ8Itens[nLinha,2],NiL})

            aadd(aLinha,{"C6_QTDVEN", aZ8Itens[nLinha,7] , Nil })

            aadd(aLinha,{"C6_PRCVEN",  nPrcVen,          Nil})

            aadd(aLinha,{"C6_PRUNIT",  0,          Nil})

            aadd(aLinha,{"C6_VALOR",   nPrcVen*aZ8Itens[nLinha,7],  Nil})    //Preço Unitário * Quantidade

            aadd(aLinha,{"C6_QTDLIB" , 0 , NiL})

            //aadd(aLinha,{"C6_LOTECTL", aZ8Itens[nLinha,5],   NiL})
            //aadd(aLinha,{"C6_LOCALIZ", aZ8Itens[nLinha,6], NiL })

            aadd(aLinha,{"C6_TES",     "501",        Nil})

            aadd(aItens, aLinha)

        Next nLinha

        nOpcX := 3 

        MSExecAuto({|a, b, c, d| MATA410(a, b, c, d)}, aCabec, aItens, nOpcX, .F.)

        If lMsErroAuto
            VTAlert("Erro na inclusao..")
            MostraErro("\system\",cDoc+".log")
            RollBackSx8()

            cDoc  := ""
        Else
            ConfirmSx8()
            //VTAlert("Pedido de Venda n-"+alltrim(cDoc)+" adicionado.","Sucesso",.T.,3000)
        Endif
    Endif

Return cDoc

//////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o TelaSZ8()                                                      //
//Monta tela vtbrowse com os itens do Z8                                //
//Retorna o item escolhido.
/////////////////////////////////////////////////////////////////////////
Static Function TelaSZ8(aZ8Itens)
    Local q
    Private nConte := 01 //contagem de cada item adicionado

    nMaxIncV := len(aZ8Itens)
    aPrep    := Array(nMaxIncV,8)
    aItx := {}
    //VTALERT("BREAKPOINT Z8-2-"+alltrim(str(nMaxIncV)))
    
    
    For q:= 1 to nMaxIncV
    
        //1-Prod,2-Desc,3-Um,4-Local,5-Lote,6-Endereço,7-Qnt,8-PesoLiquido,9-Peso Bruto,10-Num Caixas,11-Caixa inc,12-X,13-Data Entrega
        
        //1-Item,2-Prod,3-Desc,4-Um,5-Armz,6-Lote,7-Endereço,8-Qnt,9-Peso Liq,10-Peso bruto
        //VTAlert("Teste valtype:"+valtype(aZ8Itens[q,7]))
        //VTAlert("Teste:"+aZ8Itens[q,7])
        //VTAlert("Teste Bnf:"+Transform(aZ8Itens[q,7],"@R 99999999999.9999"))
        
        aPrep[q] := {strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,2],aZ8Itens[q,3],aZ8Itens[q,4],aZ8Itens[q,5],aZ8Itens[q,6],Transform(aZ8Itens[q,7],"@R 99999999999.9999"),Transform(aZ8Itens[q,8],"@R 99999999999.999"),Transform(aZ8Itens[q,9],"@R 99999999999.999")}
        Aadd(aItx,aPrep[q])
        nConte++

    End

    If nMaxIncV > 0
        aCab := {"Item    ","Produto","Descricao                ","UM         ","Armz   ","Lote    ","Endereco     ","Qnt    ","Peso Liquido    ","Peso Bruto    "} 
        aSize:= {2,TamSx3("B1_COD")[1],TamSx3("B1_DESC")[1],TamSx3("B1_UM")[1],4,TamSx3("B8_LOTECTL")[1],TamSx3("BF_LOCALIZ")[1],18,18,18}   
        nPos := 1 
        @ 0,0 VTSAY "Itens adicionados:"
        nChoose := VTaBrowse(1,0,7,31,aCab,aItx,aSize)
        VTCLEAR()
    Else
        VTALERT("Menu de itens adicionados vazio. Pressione ENTER para prosseguir.","Itens Adicionados",.T.)
        nChoose := NiL
        VTCLEAR()
    Endif


    //Cï¿½digo antigo para validaï¿½ï¿½o com qrcode.
    //Voltei ao codigo antigo pois o loop de validaï¿½ï¿½o serï¿½ feito dentro da propria funï¿½ï¿½o
    // do beepvalid.

/*     Local q
    Private nConte := 01 //contagem de cada item adicionado
    Private bTtfa  := .T.

    VTALERT("Distribuicao Lote x End","Info",.T.,1000)
    nMaxIncV := len(aZ8Itens)
    aPrep    := Array(nMaxIncV,8)
    aItx := {}
    
    // - 

    For q:= 1 to nMaxIncV
        //aPrep[q] := {Item,Produto,Descriï¿½ï¿½o,Um,Armazem,Lote,Endereco,Qnt}

        aPrep[q] := {strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,2],aZ8Itens[q,3],aZ8Itens[q,4],aZ8Itens[q,5],aZ8Itens[q,6],Transform(aZ8Itens[q,7],"@R 99999999999.9999")}
        
        bTst := BeepValid(strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,7],aZ8Itens[q,5])

        While bTtfa
            If bTst == .T.
                //Se o beep for vï¿½lido
                //VTALERT("Bp Valido!","Valido",.T.,0500)
                Aadd(aItx,aPrep[q])
                nConte++
                bTtfa := .F.
            else
                //Caso o beep seja invï¿½lido
                bMynX := MsgYesNo("Dados divergentes. Deseja tentar novamente?","Beep Invalido")
                If bMynX
                    bTst := BeepValid(strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,7],aZ8Itens[q,5])
                Else
                    VTALERT("O item nï¿½o serï¿½ adicionado.","Aviso",.T.,3000)
                    bTtfa := .F. 
                    Return
                Endif
            Endif
        Enddo

    End

    If nMaxIncV > 0
        VTCLEAR()
        aCab := {"Item    ","Produto","Descricao                ","UM         ","Armz   ","Lote    ","Endereco     ","Qnt    "} 
        aSize:= {2,TamSx3("B1_COD")[1],TamSx3("B1_DESC")[1],TamSx3("B1_UM")[1],4,TamSx3("B8_LOTECTL")[1],TamSx3("BF_LOCALIZ")[1],14}   
        nPos := 1 
        if len(aItx) == 0
            VTALERT("Nenhum item adicionado/confirmado. A tela nao sera exibida.","Aviso",.T.,3000)
        Else
            @ 0,0 VTSAY "Itens Adicionados:"
            nChoose := VTaBrowse(1,0,7,31,aCab,aItx,aSize)
            VTALERT("Len aZ8:"+str(len(aItx)),"Info")
        Endif

        VTCLEAR()
    Endif */

Return nChoose


////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o LxEMxEst()                                                   //
//Retorna o maximo disponï¿½vel em estoque baseado no lote x endereço.    // 
////////////////////////////////////////////////////////////////////////////

Static Function LxEMxEst(cCodMP)
    Private nMxEst := 0

    IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF

    cQry := "SELECT DISTINCT BF_PRODUTO AS B8_PRODUTO,BF_LOTECTL AS B8_LOTECTL,BF_LOCALIZ,BF_QUANT AS B8_SALDO,BF_EMPENHO FROM "+RETSQLNAME("SBF")+" AS SBF WHERE BF_PRODUTO = '"+cCodMP+"' AND BF_QUANT > 0 AND SBF.D_E_L_E_T_ = ' ' AND BF_LOCAL = '01' "
    TCQUERY cQry NEW ALIAS "TMP"
    TCSETFIELD("TMP","B8_DATA","D",8,0)

    DbSelectArea("TMP")
    TMP->(Dbgotop())

    While !TMP->(Eof())
        nMxEst += TMP->B8_SALDO
        TMP->(dbSkip())
    Enddo

    TMP->(dbclosearea())

Return nMxEst


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o LxEDistr()                                                                                         //
//Funcionamento similar ao LxEAdArr().                                                                      //
//ï¿½A funï¿½ï¿½o realiza consulta na query de lote x endereï¿½o com filtragem por lotes mais antigos por primeiro. //
//ï¿½Posteriormente a funï¿½ï¿½o realiza a escolha das linhas  atï¿½ que a quantidade total seja batida.
//ï¿½As linhas podem entrar na Z8 de forma desmembrada. (mesmo produto porï¿½m com as quantidades desmbembradas)
///////////////


Static Function LxEDistr(cCodMP,nQntprv,cAliasNam,cItemOrig)
    Private cArmz := "01"

    //VTALERT("Conta inc?"+cValToChar(lCntCInc))

    //Puxando Desc e UM de acordo com o código de produto
    If Select("SB1") > 0
        SB1->(DbCloseArea())
    Endif

    DbSelectArea("SB1")
    DbSetOrder(1)
    IF SB1->(DbSeek(xFilial("SB1")+cCodMP))
        //DESC
        If !empty(SB1->B1_DESC)
            cDesc := SB1->B1_DESC
        Else
            cDesc := ""
        Endif

        //Um
        If !empty(SB1->B1_UM)
            cUm := SB1->B1_UM
        Else
            cUm := ""
        Endif

        //Qnt Emb
        If !empty(SB1->B1_QE)
            nQntEmb := SB1->B1_QE
        Else
            nQntEmb := 0
            //VTALERT("Produto sem QntEmb cadastrada.","Aviso",.T.,1000)
        Endif

    Endif


/*     //Separando campos por var
    cArmz    := (cAliasNam)->LOCALX //BF_LOCAL
    cCodMP   := (cAliasNam)->PRODUTO //cCodMP
    cLote    := (cAliasNam)->LOTE //B8_LOTECTL
    cEnd     := (cAliasNam)->LOCALIZ //BF_LOCALIZ
    cSaldoli := (cAliasNam)->SALDO //B8_SALDO */

    //Puxa da tabela temporaria especifica do produto
    //ALIAS: TMP_cCodMP
    //Tabela: puxar por aScan na array aTmpNms
    
    (cAliasNam)->(DbGoTop())

    nCalc := nQntPrv

    //Primeira condição. Se verdadeira faz um loop e pega todos valores da lista. Evita perder tempo nas demais condições
    If nCalc > nMxEstoq
        While !(cAliasNam)->(Eof())
            //para linhas em que o saldo for 0.
            If (cAliasNam)->SALDO == 0
                (cAliasNam)->(DbSkip())
            Else
                //MsgAlert("Beepvalid 2")
                If BeepValid(cCodMP,(cAliasNam)->SALDO,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                    nNumcx := NoRound((cAliasNam)->SALDO / nQntEmb , 0)   //caixas completas
                    nCxInc := Mod((cAliasNam)->SALDO,nQntEmb )           //caixas incompletas

                    nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                    nPesoLli := nTmpPliq * nNumcx //total peso liq
                    
                    //Caixa incompleta:
                    //1- Se existir no calculo;
                    //2- Se habilitado pelo usuario;
                    //* Se as duas condições forem válidas, soma no volume e na quantidade.
                    
                    If nCxInc > 0 .AND. lCntCInc
                        nIncVol := 1 
                        nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb
                        nPesoLli += (nTmpPliq * nCxInc)/nQntEmb

                    Else
                        nIncVol := 0
                    EndIf

                    //* lCntcInc := Se caixa incompleta estiver ativado
                    //* Realizará contagem das caixas incompletas
                    If lCntCInc
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                    Else
                        //* Caso não esteja ativado , zera contagem das caixas incompletas. *
                        //* Solicitação Caio

                        If Empty(nCxInc)
                            nCxInc := 0
                        Endif
                        if lAddProd != .F. //se nao puder adicionar produto.. passado pelo beepvalid()
                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                        endif
                    EndIf

                    nVolTotl := nVolTotl + nNumCx + nIncVol
                    nPlTotl  := nPlTotl+nPesoLli
                    nPbTotl  := nPbTotl+nPesoBli
                    (cAliasNam)->SALDO := 0
                Endif
                (cAliasNam)->(DbSkip())
            Endif
        Enddo
        //encerra execução da função.
        Return
    Endif

    //Caso a primeira condição não seja .T., nCalc recebe nQntPrev e inicia-se a lógica seguinte.
    nCalc := nQntprv

    While nCalc != 0 .AND. !(cAliasNam)->(Eof())
        If  nCalc > (cAliasNam)->SALDO
            //1-Pega o saldo inteiro da linha
		    //2-Subtrai do nCalc 
		    //3- DbSkip()


            if lPular
                Return  
            Endif

            If (cAliasNam)->SALDO == 0
                (cAliasNam)->(DbSkip())
            Else

                //VTALERT("Beepvalid 3 lPular:"+cValToChar(lPular))

                If BeepValid(cCodMP,(cAliasNam)->SALDO,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                    nNumcx := NoRound((cAliasNam)->SALDO / nQntEmb,0)   //caixas completas
                    nCxInc := Mod((cAliasNam)->SALDO,nQntEmb)           //caixas incompletas
                    

                    nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                    nPesoLli := nTmpPliq * nNumcx //total peso liq

                    //Caixa incompleta:
                    //1- Se existir no calculo;
                    //2- Se habilitado pelo usuario;
                    //* Se as duas condições forem válidas, soma no volume e na quantidade.
                    
                    If nCxInc > 0 .AND. lCntCInc
                        nIncVol := 1 
                        nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb
                        nPesoLli += (nTmpPliq * nCxInc)/nQntEmb
                    Else
                        nIncVol := 0
                    EndIf

                    // Se Considerar Caixas Incompletas
                    If lCntCInc
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4],.F.)),lCntCInc,0.00,cTes})
                    Else
                        
                        If Empty(nCxInc)
                            nCxInc := 0
                        Endif

                        if lAddProd != .F. //Se puder adicionar produto.. passado pelo beepvalid()
                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                        endif
                    EndIf 
                    
                    nCalc := nCalc - (cAliasNam)->SALDO
                    nVolTotl := nVolTotl + nNumCx + nIncVol
                    nPlTotl  := nPlTotl+nPesoLli
                    nPbTotl  := nPbTotl+nPesoBli
                    (cAliasNam)->SALDO := 0
                
                    (cAliasNam)->(DbSkip())
                Endif
            Endif

        Elseif nCalc <= (cAliasNam)->SALDO

            //Adiciona o valor total do nCalc e encerra execução.
            //VTALERT("beepvalid 4 ")
            If BeepValid(cCodMP,nCalc,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                nNumcx := NoRound(nCalc / nQntEmb , 0)   //caixas completas
                nCxInc := Mod(nCalc,nQntEmb )           //caixas incompletas

                nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                nPesoLli := nTmpPliq * nNumcx //total peso liq

                //Caixa incompleta:
                //1- Se existir no calculo;
                //2- Se habilitado pelo usuario;
                //* Se as duas condições forem válidas, soma no volume e na quantidade.
                
                If nCxInc > 0 .AND. lCntCInc
                    nIncVol := 1 
                    nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb //regra de tres puxando peso bruto de acordo com qnt caixa inc
                    nPesoLli += (nTmpPliq * nCxInc)/nQntEmb
                Else
                    nIncVol := 0
                EndIf
                
                //Se considerar caixas incompletas.
                If lCntCInc
                    Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nCalc,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                Else

                    If Empty(nCxInc)
                        nCxInc := 0
                    Endif

                    if lAddProd != .F. //Se puder adicionar produto.. passado pelo beepvalid()
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nCalc-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc,0.00,cTes})
                    endif
                Endif
                

                nVolTotl           := nVolTotl + nNumCx + nIncVol
                nPlTotl            := nPlTotl+nPesoLli
                nPbTotl            := nPbTotl+nPesoBli
                nSobra             := (cAliasNam)->SALDO - nCalc
                nCalc              := nCalc - nCalc
                (cAliasNam)->SALDO := nSobra
                
                //ImpZRsto(cCodMP,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,nSobra,nTmpPliq) //Imprime Sobra Etiqueta Zebra.
                VTCLEAR()
            Endif
            Return
        Endif
    Enddo

Return


////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o BeepQr(aItens)                                      //
//Beep para soma do volume,caixa,peso bruto e peso lï¿½quido  //
/////////////////////////////////////////////////////////////

Static Function BeepQr(aItens)  
    Local q
    Private aIty        := {}
    Private nConte      := 01
    Private bLpBeep     := .T.
    Private cBpqr       := SPACE(45) //String principal beep qrcode
    Private cQtCx       := SPACE(10)
    Private cQtCxInc    := SPACE(10)
    Private cQtPrcx     := SPACE(10)
    Private cPbruto     := SPACE(10)
    Private cPliquido   := SPACE(10)
    Private cDelimit    := "$"

    VTCLEAR()        
    @ 0,0 VTSAY "Item - "+StrZero(nChoose,2)
    @ 1,0 VTSAY "QrCode"
    @ 2,0 VTSAY "QR:" VTGET cBpQr VALID MxBpQnt(cBpQr,cChoose,aIty) == .T.
    VTREAD
    VTCLEAR()

    //Ordem das informações do QrCode:
    //Produto, Qnt,Lote, Peso Líquido, Peso Bruto

    //Separar Dados QrCode
    nSearch := AT(cDelimit,cBpQr)
    //Produto
    nAx      := AT(cDelimit,cBpQr,1)
    cQcprod  := Substring(cBpQr,0,nAx-1) //pegando do primeiro ate o ultimo simbolo
    cMainStr := Substring(cQxprod,,nAx+1,len(cQcprod))
    cProd := PADR(cQcprod,15)
    //Qnt
    nQx      := AT(cDelimit,cMainStr,1)
    cQnt     := Substring(cMainStr,0,nQx-1)
    cMainStr := Substring(cMainStr,nQx+1,len(cMainStr))
    nQtde    := cQnt

    //Lote
    nLx      := AT(cDelimit,cMainStr,1)
    cQLote   := Substring(cMainStr,0,nLx-1)
    cMainStr := Substring(cMainStr,nLx+1,len(cMainStr))
    cLote    := PADR(cQLote,10)

    //Peso Liquido
    nMx      := AT(cDelimit,cMainStr)
    cQPesol  := Substring(cMainStr,0,nLx-1)
    cMainStr := Substring(cMainStr,nMx+1,len(cMainStr))
    cPesol   := PADR(cQPesol,10)

    //Peso Bruto
    nZx      := AT(cDelimit,cMainStr)
    cQPesob  := Substring(cMainStr,0,nZx-1)

Return


//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//@Funï¿½ï¿½es de validaï¿½ï¿½o:@@
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


//Validação Fornecedor
Static Function ValFornec(cCodF)
    Private lRt

    If Select("SA2") > 0
        SA2->(DbCloseArea())
    Endif

    DbSelectArea("SA2")
    DbSetOrder(1)
    If SA2->(DbSeek(xFilial("SA2")+cCodF))
        //DbSeek retornou
        //VTAlert("Fornecedor inexistente.")
        lRt := .T.
    Else
        //DbSeek nao retornou.. 
        //VTAlert("Fornecedor inexistente.")
        lRt := .F.
    Endif 



Return lRt

//Validação Endereço
Static Function ValEnd(cEnd)
    Private lRt

    If Select("SBE") > 0
        SBE->(DbCloseArea())
    Endif

    DbSelectArea("SBE")
    DbSetOrder(9)
    If DbSeek(xFilial("SBE")+padr(cEnd,TamSx3('BE_LOCALIZ')[1]))
        //VTAlert("Endereco existente.")
        Return lRt := .T.
    Else
        VTAlert("Endereco Inexistente.")
        cEnd := SPACE(15)
        Return lRt := .F.
    Endif
    
Return lRt


Static Function ValPVenda(cPVenda)
    DbSelectArea("SC6")
    DbSetOrder(1)
    If SC6->(DbSeek(xFilial("SC6")+cPVenda))
        Return .T.
    else
        //VtAlert("Pedido de venda inválido.")
        Return .F.
    Endif

Return

////////////////////////////////////////////////////////////////////////
//Quantidade BeepQr                                                   //
//A quantidade beepada nao pode ser superior a quantidade da linha    //
////////////////////////////////////////////////////////////////////////

Static Function MxBpQnt(cBpQr,cChoose,aIty) 
    
    Local bMxBpQnt
    

    If !empty(cBpQr)
        nLQnt := aIty[nChoose,8]

        //cTstX := Valtype(cBpQr)   C
        //cTstY := Valtype(cLQnt)   N
        //VTALERT("cTstX:"+cTstX+"-cTsty:"+cTstY)

/*         If val(cBpQr) <= cLQnt
            //Tudo ok, quantidade menor ou igual.
            bMxBpQnt := .T.
        Else
            VTALERT("Quantidade beepada excede o total do item "+cChoose+".","Erro Qnt",.T.,3000)
            bMxBpQnt := .F.    
        Endif */

        nRes := val(cBpQr) - cLQnt
        //VTALERT("nLQnt:"+nLQnt)
        If nRes != 0 //se o resto nao for zero, significa que as quants sao diferentes.
            //VTALERT("A quantidade beepada "+cBpQr+" e "+str(cLQnt)+" sao divergentes.")
            @ 0,0 VTSAY "A quantidade beepada ("+alltrim(cBpQr)+") e QntLinha ("+TRANSFORM(nLQnt,"@R 99999999999.9999")+") sao divergentes."
            @ 1,0 VTSAY "Deseja continuar?"
            nOp:=VTaChoice(2,0,6,VTMaxCol(),{"Sim","Nao"})
            If nOp == 1
                bMxBpQnt := .T.
                Return
            elseif nOp == 2
                bMxBpQnt := .F.
                Return
            Endif 
        else
            bMxBpQnt := .T.
        Endif
    Else
        bMxBpQnt := .F.    
    Endif

Return bMxBpQnt

//////////////////////////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o para confirmaï¿½ï¿½o do Prod/Lote.                                                         //
//Retorna .T. caso o produto/lote seja o mesmo do Qr beepado - Retorna .F. caso nï¿½o seja.       // 
//////////////////////////////////////////////////////////////////////////////////////////////////

Static Function BeepValid(cZProd,cZqnt,cZlote,cZLocal,cZQntEmb,lCntCInc)
   
    Private bIsBpVld := .F. //booleano final. Caso .F., o item nï¿½o ï¿½ adicionado a array final e o programa entra em loop.
    Private cBpQr    := SPACE(45)
    Private cDelimit := "$"
    Private bValProd := .F.
    Private bValQnt  := .F.
    Private bValLote := .F.
    Private bIsEdVld := .F.
    Private bLoop    := .T.
    Private cEndCnf  := SPACE(TamSx3('BF_LOCALIZ')[1])
    Private nZvol    := 0
    Private nCxInz   := 0
    Private nIncVolz := 0
    Private nZvoltl  := 0
    Private nVoltt   := 0 //Volume total
    Public nTmpPliq  := 0 //Peso Lï¿½quido Temporï¿½rio. Para inclusao na array da lista final
    Public nTmpPbrt  := 0

    lAddProd := .T. //reset addprod
    //Calculo volumes
    nZvol  := NoRound(cZqnt / cZQntEmb , 0)   //caixas completas
    nCxInz  := Mod(cZqnt,cZQntEmb )           //caixas incompletas

    //Para casos em que a opção "Considerar Caixa Incompleta" estiver desativado
    //e houver somente caixas incompletas.
    //VTALERT("cZqnt:"+str(cZqnt)+"-cZQntEmb:"+str(cZQntEmb)+"-lCntciNC"+cValToChar(lCntCInc))

    //caso a quantidade a beepar seja somente caixa incompleta e a opcao "consid caixa inc?" for "não".
    If  cZqnt-nCxInz <= 0.0000 .AND. nCxInz != 0.000 .AND. !lCntCInc
        lAddProd := .F.
        Return bIsBpVld := .T.    
    Else


        //Considera tambem caixas incompletas como vol + 1
        If nCxInz > 0 .AND. lCntCInc
            nIncVolz += 1
        Else
            //caso contrario passa qnt vol caixa inc zerada
            nIncVolz := 0
        Endif

        nZvoltl := nZvol +nIncVolz

        While bLoop
            cError := ""
            cZxprod := padr(cZProd,TamSx3("B1_COD")[1])
            cZxlote := padr(cZxlote,TamSx3("B8_LOTECTL")[1])

            VTCLEAR()
            @ 0,0 VTSAY "-Confirme o Item-"
            @ 1,0 VTSAY "Prod - "+cZProd

            If lCntCInc
                @ 2,0 VTSAY "Qnt  - "+alltrim(Transform(cZqnt,"@R 99999999999.9999"))
            Else
                @ 2,0 VTSAY "Qnt  - "+alltrim(Transform(cZqnt-nCxInz,"@R 99999999999.9999"))        
            Endif


            @ 3,0 VTSAY "Lote - "+cZLote
            @ 4,0 VTSAY "End  - "+cZLocal
            @ 5,0 VTSAY "Vol  - "+alltrim(str(nZvoltl))
            @ 6,0 VTSAY "---------------------------"
            @ 7,0 VTSAY "QR:" VTGET cBpQr
            VTREAD

            cQxProd := cBpQr

            //nSearch = >0 para qrcode / 0 para nï¿½o QrCode.
            nSearch := AT(cDelimit,cQxProd)
            
            If nSearch != 0
                
                //Separação de Informações do QrCode
                //Produto
                nAx      := AT(cDelimit,cQxProd,1)
                cQcprod  := Substring(cQxProd,0,nAx-1) //pegando do primeiro ate o ultimo simbolo
                cMainStr := Substring(cQxProd,nAx+1,len(cQxProd))
                cProd    := PADR(cQcprod,TamSx3("B1_COD")[1])

                //Qnt
                nQx      := AT(cDelimit,cMainStr,1)
                cQnt     := Substring(cMainStr,0,nQx-1)
                cMainStr := Substring(cMainStr,nQx+1,len(cMainStr))
                nQtde    := cQnt
                
                //Lote
                nLx      := AT(cDelimit,cMainStr,1)
                cQLote   := Substring(cMainStr,0,nLx-1)
                cMainStr := Substring(cMainStr,nLx+1,len(cMainStr))
                cLote    := PADR(cQLote,TamSx3("B8_LOTECTL")[1])

                //Peso Líquido
                nYx      := AT(cDelimit,cMainStr,1)
                cQPesol  := Substring(cMainStr,0,nYx-1)
                cMainStr := Substring(cMainStr,nYx+1,len(cMainStr))
                cPesol   := PADR(cQPesol,TamSx3("B1_PESO")[1])

                //Peso Bruto
                nZx      := AT(cDelimit,cMainStr,1)
                cQPesob  := Substring(cMainStr,0,nZx-1)
                cMainStr := Substring(cMainStr,nZx+1,len(cMainStr))
                cPesob   := PADR(cQPesob,TamSx3("B1_PESO")[1])

                //Utilização do StrTran para corrigir bug nas casas decimais peso liquido e peso bruto.
                //Solução feita no ACD para evitar alteração da etiqueta zebra de novo.    
                //StrTran( < cString >, < cSearch >, [ cReplace ], [ nStart ], [ nCount ] )   

                cSearch  := "," //caracter a pesquisar
                cReplace := "." //caracter a substituir
                cPesol := StrTran(cPesol,cSearch,cReplace) //troca , por . no Peso Liquido.
                cPesob := StrTran(cPesob,cSearch,cReplace) //troca , por . no Peso Bruto.
                
                VTALERT("Produto:"+cProd+"-Qnt:"+nQtde+"-Lote:"+cLote+"-Peso Liq:"+cPesol+"-Peso Bruto:"+cPesob,"Info QrCode",.T.,4000)
                VTCLEAR()

                
                //Pular ou nao a tela do beep endereço.
                lPula := .F.

                //Caso der erro, já vai para tela do erro antes de rodar o beep endereço.
                If alltrim(cProd) != alltrim(cZProd) .OR. alltrim(cLote) != alltrim(cZLote)
                    //VTALERT("Produto ou Lote Invalido.","Erro Beep",.T.,3000)
                    lPula := .T.
                Else
                    lPula := .F.
                EndIf

                If !lPula
                    @ 0,0 VTSAY "-Confirme o Endereco-"
                    @ 1,0 VTSAY "End  - "+cZLocal
                    @ 2,0 VTSAY "---------------------------"
                    @ 3,0 VTSAY "Beep:" VTGET cEndCnf
                    VTREAD
                    
                    //Checa se o endereço é valido. Caso nao seja, se faz necessário beepar
                    //tudo novamente.
                    If alltrim(cEndCnf) == alltrim(cZLocal)
                        VTALERT("Endereço confirmado:"+alltrim(cZLocal)+".","Ok",.T.,1000)
                        bIsEdVld  := .T.
                        bIsBpVld  := .T.
                    Else
                        //VTBEEP(2)
                        VTALERT("Erro ao confirmar o endereço. Beep invalido.","Erro",.T.,2000)
                        bIsEdVld  := .F.
                        bIsBpVld  := .F.
                    Endif

                Endif
            
                //Compara dados do QrCode com os dados 
                //Caso houver algum dado divergente, volta para tela do adicionar mais itens.
                If alltrim(cProd) == alltrim(cZProd) .AND. alltrim(cLote) == alltrim(cZLote) .AND. bIsEdVld
                    //confirmaï¿½ï¿½o prod e lote
                    VTBEEP(1)
                    nTmpPliq  := val(cPesol)+nTmpPliq //Somatoria Peso Liquido.. Só soma se confirmar prod+lote.
                    nTmppbrt  := val(cPesob)+nTmpPbrt //Somatória Peso Bruto.. Só soma se confirmar prod+lot.
                    bLoop    := .F.
                    VTCLEAR()
                Else
                    //--Verifica qual das informaï¿½ï¿½es estï¿½ divergente--
                    VTBEEP(2)
                    
                    //Valida Produto
                    If alltrim(cProd) != alltrim(cZProd)
                        //VTALERT("Beep invalido. Erro Produto.","Beep Invalido",.T.,2000)
                        cError += "Produto,"
                    Endif
                    
                    //Valida Lote
                    If alltrim(cLote) != alltrim(cZLote)
                        //VTALERT("Beep Invalido. Erro Lote.","Beep Invalido",.T.,2000)
                        cError += "Lote,"
                    Endif

                    //Se existir algum erro.
                    If len(cError) > 1
                        VTALERT("Beep invalido. Erro:"+cError,"Beep Invalido.",.T.,3000)
                    Endif

                    VTCLEAR()
                    @ 0,0 VTSAY "Dados divergentes."
                    @ 1,0 VTSAY "Tentar novamente?"
                    nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                    If nOp == 1
                        //reset string qr
                        cBpQr   := SPACE(45)
                        cEndCnf := SPACE(TamSx3('BF_LOCALIZ')[1])
                        VTALERT("Aguarde..","Carregando",.T.,500)
                        lPular := .T.
                    
                    Else
                        VTALERT("A linha nao sera adicionada.","Aviso",.T.,3000)
                        VTCLEAR()


                        //***TIRA OS ITENS QUE NAO FORAM ADICIONADOS DA ARRAY DE TRAVA*****
                        //->Modificação 13092022 - Arinusk

                        If nPosIc != 0
                            nPosIc := aScan(aReadyad,cItemAt)
                            //Deleta item que nao foi adicionado da array
                            aDel(aReadyAd,nPosIc)
                            //Redimensiona o Array
                            aSize(aReadyAd, Len(aReadyAd)-1)
                        Endif

                        lPular := .T.

                        Return bIsBpVld := .F.
                    Endif

                EndIf

            Else
                VTBEEP(2)
                VTCLEAR()
                @ 0,0 VTSAY "QR invalido."
                @ 1,0 VTSAY "Tentar novamente?"
                nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                If nOp == 1
                    VTCLEAR()
                    cBpQr := SPACE(45)
                    cEndCnf := SPACE(TamSx3('BF_LOCALIZ')[1])
                    VTALERT("Carregando..","Aguarde",.T.,500)
                Else
                    VTALERT("A linha nao sera adicionada.","Aviso",.T.,3000)
                    VTCLEAR()

                    //***TIRA OS ITENS QUE NAO FORAM ADICIONADOS DA ARRAY DE TRAVA*****
                    If nPosIc != 0
                        nPosIc := aScan(aReadyad,cItemAt)
                        //Deleta item que nao foi adicionado da array
                        aDel(aReadyAd,nPosIc)
                        //Redimensiona o Array
                        aSize(aReadyAd, Len(aReadyAd)-1)
                    Endif


                    lPular := .T.

                    Return bIsBpVld := .F.

                Endif

            Endif
        Enddo

    Endif

Return bIsBpVld


/////////////////////////////////////////////
//Beep QrCode na inclusao de picklist      //
// do tipo beneficiamento                  //
////////////////////////////////////////////

Static Function BeepBnf()
    Private lRt
    Private cDelimit := "$"
    Private lLp      := .T.
    Private cUm      := "" //U.M produto
    Private nQtEmb   := SB1->B1_QE
    //Arrays
    Private aBpbnf := {} //Produto, Qnt, Lote,Peso Bruto

    While lLp
        //Qr: Produto,Lote,Qnt
        @ 0,0 VTSAY "Adicionar Item:"
        @ 1,0 VTSAY "----------------"
        @ 2,0 VTSAY "QR:" VTGET cBqrc 
        VTREAD
        VTCLEAR()

        cQxProd := cBqrc

        nSearch := AT(cDelimit,cQxProd)

        If nSearch != 0
            //Separação de Informações do QrCode
            
            //Produto
            nAx      := AT(cDelimit,cQxProd,1)
            cQcprod  := Substring(cQxProd,0,nAx-1) //pegando do primeiro ate o ultimo simbolo
            cMainStr := Substring(cQxProd,nAx+1,len(cQxProd))
            cProd    := PADR(cQcprod,TamSx3("B1_COD")[1])

            //Qnt
            nQx      := AT(cDelimit,cMainStr,1)
            cQnt     := Substring(cMainStr,0,nQx-1)
            cMainStr := Substring(cMainStr,nQx+1,len(cMainStr))
            nQtde    := cQnt
            
            //Lote
            nLx      := AT(cDelimit,cMainStr,1)
            cQLote   := Substring(cMainStr,0,nLx-1)
            cMainStr := Substring(cMainStr,nLx+1,len(cMainStr))
            cLote    := PADR(cQLote,TamSx3("B8_LOTECTL")[1])

            //Peso Bruto - A4 de Recebimento
            
            VTBEEP(1)
            
            VTALERT("Prod:"+cProd+"-Qtd:"+nQtde+"-Lote:"+cLote,"Valido",.T.,3000)

            If Select("SB1") > 0
                SB1->(DbCloseArea())
            Endif
            DbSelectArea("SB1")
            DbSetOrder(1) //Filial+Cod
            If !SB1->(DbSeek(xFilial("SB1")+cProd))
                VTAlert("Produto nao existente.","Pressione Enter")
                Return NiL
            EndIf

            aAdd(aBpBnf,{cProd,val(nQtde),cLote})
            
            //Caso o produto for adicionado..
            lLp := .F.
            //lRt := .T.

        Else
            //N achou..
            VTBEEP(2)
            VTCLEAR()
            @ 0,0 VTSAY "QR invalido."
            @ 1,0 VTSAY "Tentar novamente?"
            nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
            If nOp == 1
                VTCLEAR()
                cBqrc := SPACE(45)
                VTALERT("Carregando..","Aguarde",.T.,500)
                lLp := .T.
            Else
                VTALERT("Produto não será adicionado.","Aviso",.T.,3000)
                VTCLEAR()
                //lRt := .F.
                lLp := .F.
                Return NiL
            EndIf

        Endif

    EndDo


Return aBpbnf


/////////////////////////////////////////
//Função para Envio de Email Automatico//
////////////////////////////////////////

Static Function AutoEml(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)
    Local cAcnt		:= ""
	Local cServer	:= ""
	Local cPsw		:= ""
    Private cCodC   := ""

    DbSelectArea("SC5")
    SC5->(DbSetOrder(1))
    If DbSeek(xFilial("SC5")+cPddv)
        cCodC := SC5->C5_CLIENTE
    Endif
    SC5->(DbCloseArea())

    DbSelectArea("SZ7")
    SZ7->(DbSetOrder(1))
    If DbSeek(XFilial("SZ7")+cNumPL)

        //Qnt Volume
        If !empty(SZ7->Z7_QVOL)
            nQntVol := SZ7->Z7_QVOL
        Else
            nQntVol := 0.0000
        Endif

        //Total Peso Lï¿½quido
        If !empty(SZ7->Z7_PESOL)
            nPesL := SZ7->Z7_PESOL
        Else
            nPesL := 0.0000    
        Endif

    Endif

	cAcnt		:= Alltrim(GETMV("MV_EMCONTA"))
	cServer	    := Alltrim(GETMV("MV_RELSERV"))
	cPsw		:= Alltrim(GETMV("MV_EMSENHA"))
    cMlTo       := Alltrim(GETMV("MV_PLSMLTO"))

	cHtml := ObtemHtm(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)
	lResulSend := .T.
	lEnvio := .T.

	CONNECT SMTP SERVER Alltrim(cServer) ACCOUNT Alltrim(cAcnt) PASSWORD Alltrim(cPsw) RESULT lEnvio 
	If !lEnvio 
		 MsgStop("Erro na emissao do E-mail (Conexao) !!!") 
	Else
		//cTo := "Arinus@NewCompton.com.br ;"
		//cTo := "Arinus@NewCompton.com.br ; fiscal@msadobrasil.com.br ; fiscal2@msadobrasil.com.br ; logistica@msadobrasil.com.br ; caio@msadobrasil.com.br ; almoxarifado2@msadobrasil.com.br;"
        cTo   := cMlTo
        cCC := ""
		MailAuth(Alltrim(cAcnt),Alltrim(cPsw))
		SEND MAIL FROM Alltrim(cAcnt) TO cTo CC cCC SUBJECT "[ACD]Notificação Automática - Picklist de Saída N-"+alltrim(cNumPl) BODY cHtml RESULT lResulSend
	Endif

	If !(lResulSend) 
		_cMailError := ""
		Get Mail Error _cMailError 
		Alert("Não foi possível enviar o email. Erro: "+ _cMailError) 
	EndIf 

	DISCONNECT SMTP SERVER

Return             


Static Function ObtemHtm(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)

	Local   cRet := ""
    Local   W,x
    Private nMaxIt := len(aZ8Insrt) //Total de itens
    Private nBQe //Qnt por embalagem
    Private nNumCx := 0
    Private nCxInc := 0

	//Fixo
	cRet += "<!DOCTYPE html>"
	cRet += "<html>"
	cRet += "<head>"
	cRet += "<style>"
	cRet += "table, th, td {"
	cRet += "border: 1px solid black;"
	cRet += "border-collapse: collapse; 
	cRet += "}"
	cRet += "</style>"
	cRet += "</head>"
	cRet += "<body>"
	cRet += "<h1> Notificação Automática - Picklist de Saída </h1>"
	//cRet += "<h3> Cabeçalho:</h3>"	
	cRet += "<table>"
	cRet += "<tr>"
    cRet += "<th> Cod. Cliente </th>"
	cRet += "<th> Num Picklist </th>"
	cRet += "<th> Data </th>"
	cRet += "<th> Hora </th>"
	cRet += "<th> Solicitante </th>"
	cRet += "<th> Num PdV </th>"
    cRet += "<th> Status </th>"
    cRet += "<th> Natureza Operação </th>"
	cRet += "</tr>"

	//Dinamico - Cabeçalho Picklist
	cRet += "<tr>"
    cRet += "<td> "+PADR(cCodC,TamSx3('C5_CLIENTE')[1])+" </td>"
	cRet += "<td> "+PADR(cNumPl,TamSx3("Z7_NUM")[1])+" </td>"
	cRet += "<td> "+cData+" </td>"
	cRet += "<td> "+cHora+" </td>"
	cRet += "<td> "+PADR(cUsuario,TamSx3("Z7_SOLIC")[1])+" </td>"
	cRet += "<td> "+PADR(cPddv,TamSx3("Z7_PEDIDO")[1])+" </td>"

    //Status Picklist
    If cStatus == "A"
        cRet += "<td> Aberto </td>"
    Elseif cStatus == "F"
        cRet += "<td> Faturado </td>"
    Endif
    
    //Natureza Operação
    If cNatOp == "V"
        cRet += "<td> Venda </td>"
    Elseif cNatOp == "B"
        cRet += "<td> Beneficiamento </td>"
    Elseif cNatOp == "D"
        cRet += "<td> Devolução </td>"
    Elseif cNatOp == "R"
        cRet += "<td> Retrabalho </td>"
    Elseif cNatOp == "O"
        cRet += "<td> Outras Saídas </td>"
    Endif
    
	cRet += "</tr>"
	cRet += "</table>"

    //Dinamico - Itens Picklist
    cRet += "<h3>Itens:</h3>"
    cRet += "<table>"
    cRet += "<tr>"
    cRet += "<th>Item</th>"
    cRet += "<th>Código Produto</th>"
    cRet += "<th>Descrição</th>"
    cRet += "<th>U.M</th>"
    cRet += "<th>Armazém</th>"
    cRet += "<th>Localização</th>"
    cRet += "<th>Lote</th>"
    cRet += "<th>Quantidade</th>"
    cRet += "<th>Peso Líquido</th>"
    cRet += "<th>Peso Bruto</th>"
    cRet += "<th>Caixas Completas</th>"
    cRet += "<th>Caixa Incompleta (Qnt)</th>"
    cRet += "<th>Data Entrega</th>"
    cRet += "</tr>"
    
    //cCodMp,cDesc,cUm,cArmz,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,TMP->B8_SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc
    For W := 1 To nMaxIt

        cRet += "<tr>"
        cRet += "<td>"+strzero(W,2)+"</td>" //item
        cRet += "<td>"+aZ8Insrt[w,1]+"</td>" //Código Produto
        cRet += "<td>"+aZ8Insrt[w,2]+"</td>" //Descrição
        cRet += "<td>"+aZ8Insrt[w,3]+"</td>" //U.M
        cRet += "<td>"+aZ8Insrt[w,4]+"</td>" //Armazém
        cRet += "<td>"+aZ8Insrt[w,6]+"</td>" //Lote
        cRet += "<td>"+aZ8Insrt[w,5]+"</td>" //end
        cRet += "<td><font color='blue'><b>"+Transform(aZ8Insrt[w,7],"@R 99999999999.999")+"</b></font></td>" //saldo
        cRet += "<td>"+Transform(aZ8Insrt[w,8],"@R 99999999999.999")+"</td>" //peso liquido
        cRet += "<td>"+Transform(aZ8Insrt[w,9],"@R 99999999999.999")+"</th>" //peso bruto
        cRet += "<td>"+alltrim(str(aZ8Insrt[w,10]))+"</td>" //caixa comp
        cRet += "<td>"+Transform(aZ8Insrt[w,11],"@R 99999999999.999")+"</th>" //caixa inc
        cRet += "<td>"+aZ8Insrt[w,13]+"</td>"
        cRet += "</tr>"
    End

    cRet += "</table>"
    cRet += "<h3>Resumo de Carga:</h3>"
    cRet += "<table>"
    cRet += "<tr>"
    cRet += "<th>Total Volume</th>"
    cRet += "<th>Total Peso Líquido</th>"
    cRet += "<th>Total Peso Bruto </th>"
    cRet += "</tr>"
    cRet += "<tr>"
    cRet += "<td>"+cValToChar(nVolTotl)+"</td>"
    cRet += "<td>"+Transform(nPlTotl,"@R 99999999999.999")+"</td>"
    cRet += "<td>"+Transform(nPbTotl,"@R 99999999999.999")+"</td>"
    cRet += "</tr>"
    cRet += "</table>"
    cRet += "</body>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<footer><h9><b>***E-mail Automático - Sistema Protheus - Não responder***</b></h9></footer>"

Return cRet


////////////////////////////////////////////
//Checa itens duplicados                  //
// Retorna .T. para dupl e .F. para n dupl//
////////////////////////////////////////////

Static Function CheckDup(cItem,aReadyad)
    Local nPos
    Private bCheck

    nPos := aScan(aReadyad,cItem)

    If nPos == 0
        bCheck := .F.
    Else
        bCheck := .T.   
    Endif

Return bCheck

///////////////////////////////////////////////////////////////////
//Função de Impressão de Etiqueta A4 de Estoque                 //
//Imprime Etiqueta A4 de Estoque de saldo Remanescente Spool   //
//@Parametros: Produto,Lote,Endereço,Saldo.                   //
///////////////////////////////////////////////////////////////

Static Function ImpZRsto(cProd,cLote,cLocaliz,nSobra,nTmpPliq,cCliMan)
    //////////////////////////////////////
    //Parametro de Escolha Impressora.  //
    //MV_IMPCHS1 = Impressora 1           //
    //MV_IMPCHS2 = Impressora 2           //
    //////////////////////////////////////
    Local X
    local cRelName as char
    local lAdjust as logical
    local nPrintType as numeric
    local oFont10 as object
    local oPrinter as object
    local cText as char
    local nSize as numeric
    Private cCliMan  := SPACE(15) 
    Private cDescrip := ""
    Private aTst     := {}
    //PUTMV("MV_IMPCHS1","")
    //PUTMV("MV_IMPCHS2","") 
    
    cImpOne := GetMV("MV_IMPCHS1") //Pega o endereço da impressora 1.
    cImpTwo := GetMV("MV_IMPCHS2") //Pega o endereço da impressora 2.

    //VTALERT("Imp1:"+alltrim(cImpOne)+"-Imp2:"+alltrim(cImpTwo))

    VTCLEAR()

    //Como será a4 de receb, não é necessário.
/*     @ 0,0 VTSAY "Informacoes:"
    @ 1,0 VTSAY "Cliente:" VTGET cCliMan
    VTREAD */

    VTCLEAR()
    @ 0,0 VTSAY "Selecione a impressora:"
    nOp:=VTaChoice(1,0,6,VTMaxCol(),{"Logistica","Estoque"})

    If nOp == 1
        //Impressora 1
        cPrinter := "RICOH MP C3003"

    Elseif nOp == 2
        //Impressora 2
        cPrinter := GetMV("MV_IMPCHS2")
    Endif

    cCodmsa  := alltrim(cProd) //Codigo Msa (Produto)
        
    //descrição
    DbSelectArea("SB1")
    SB1->(DbSetOrder(1))
    If SB1->(DbSeek(xFilial()+mv_par02))

        //Puxando Descrição Do Produto
        If !Empty(SB1->B1_DESC)
            cDescrip :=  SB1->B1_DESC
        else
            cDescrip := ""
        Endif

        //Puxando Código Cliente
        If !Empty(SB1->B1_XXCODCL)
            cCodCli := SB1->B1_XXCODCL
        Else
            cCodCli := ""
        Endif

    Endif

    cQntde   := Transform(nSobra,"@E 999,999.999")
    cLote    := alltrim(cLote) // Lote
    cLmatp   := "" // Lote Materia Prima
    cDatemb  := "99/99/99" // Data da Embalagem
    cPesobrt := "1 KG"

    cPesoliq := Transform(nTmpPliq,"@E 999,999.999")

    cCliente := alltrim(cCliMan)

    cRelName   := "EtqEstoque"+alltrim(str(Random(0,999)))
    lAdjust    := .F.
    nPrintType := 2  //IMP_SPOOL

    oFont05    := TFont():New("ARIAL",05,05,,.F.,,,,,.F.,.F.) ///Fonte 5 Normal
    oFont07    := TFont():New("ARIAL",07,07,,.F.,,,,,.F.,.F.) ///Fonte 7 Normal
    oFont10    := TFont():New("ARIAL",10,10,,.F.,,,,,.F.,.F.) ///Fonte 10 Normal
    oFont15    := TFont():New("ARIAL",15,15,,.F.,,,,,.F.,.F.) ///Fonte 15 Normal
    oFont17    := TFont():New("ARIAL",17,17,,.F.,,,,,.F.,.F.) ///Fonte 17 Normal
    oFont20    := TFont():New("ARIAL",20,20,,.F.,,,,,.F.,.F.) ///Fonte 20 Normal
    oFont25    := TFont():New("ARIAL",25,25,,.F.,,,,,.F.,.F.) ///Fonte 25 Normal
    oFont30    := TFont():New("ARIAL",30,30,,.F.,,,,,.F.,.F.) ///Fonte 30 Normal
    oFont35    := TFont():New("ARIAL",35,35,,.F.,,,,,.F.,.F.) ///Fonte 35 Normal

    oFont25b   := TFont():New("ARIAL",25,25,,.T.,,,,,.F.,.F.) ///Fonte 25 Normal

    oPrinter := FWMSPrinter():New(cRelName, nPrintType , lAdjust, "/SPOOL/", .T., /*lTReport*/ ,,cPrinter, .F. ,)
    //oPrint := FwMsPrinter():New('NOME RELATORIO', IMP_SPOOL, .T.,,.T.,,,'NOME_IMPRESSORA')
    oPrinter:SetLandscape()
    oPrinter:SetPaperSize(DMPAPER_A4)
    oPrinter:setCopies(1)
    
    
    nCenterPg := Round(oPrinter:nHorzSize() / 2 ,0)

    oPrinter:StartPage() // Inicia uma nova pagina
    oPrinter:SetParm( "-RFS")

    //Construção do Layout - Boxes.
    //FWMsPrinter(): Box ( < nRow>, < nCol>, < nBottom>, < nRight>, [ cPixel] ) -->
    oPrinter:Box( 15, 5, 600, 836, "-2")
    oPrinter:Box( 15, 5, 200, 200, "-2")
    oPrinter:Box( 200, 5, 400, 200, "-2")
    oPrinter:Box( 400, 5, 581, 200, "-2")

    //Construção do Layout - Linhas
    //FWMsPrinter(): Line ( < nTop>, < nLeft>, < nBottom>, < nRight>, [ nColor], [ cPixel] ) -->

    oPrinter:Line( 107, 200, 107, 836)
    oPrinter:Line( 200, 200, 200, 836)
    oPrinter:Line( 285, 200, 285, 836)
    oPrinter:Line( 370, 200, 370, 836)
    oPrinter:Line( 400, 200, 400, 836)

    //rodapé
    oPrinter:Line( 581, 5, 581, 836)

    oPrinter:SayBitmap( 27,27 , "/SYSTEM/LGMID.PNG",150,150)
    
    //Fixos - Say
    oPrinter:Say(210, 85, "Qr Code",oFont10)
    oPrinter:Say(410, 70, "Esboço de Peça",oFont10)
    oPrinter:Say(595,260,"M.S.A do Brasil Ltda - São José dos Campos - SP - www.msadobrasil.com.br",oFont10)
    oPrinter:Say(30,450,"A4 Estoque",oFont10)
    
    //Fixo + Var+Codebar
    oPrinter:Say(66,215,"Código Cliente",oFont25)
    oPrinter:Say(85,215,"(Part Number Customer)",oFont15)
    oPrinter:Say(66,395,cCodCli,oFont20)
    oPrinter:Code128(73, 396,cCodCli, 1, 27)
    
    oPrinter:Say(155,215,"Código M.S.A",oFont25)
    oPrinter:Say(174,215,"(Part Number M.S.A)",oFont15)
    oPrinter:Say(155,395,cCodMsa,oFont20)
    oPrinter:Code128(162,396,cCodMsa, 1, 27)

    oPrinter:Say(240,215,"Descrição",oFont25)
    oPrinter:Say(255,215,"(Description)",oFont15)
    oPrinter:Say(240,395,cDescrip,oFont20)

    oPrinter:Say(330,215,"Quantidade",oFont25)
    oPrinter:Say(349,215,"(Quantity)",oFont15)
    //oPrinter:Say(330,395,alltrim(Transform(cQntde,"@E 999,999.999")),oFont20)
    //oPrinter:Code128(337,396,alltrim(Transform(cQntde,"@E 999,999.999")), 1, 27)


    oPrinter:Say(390,215,"Cliente",oFont15)
    oPrinter:Say(398,215,"(Customer)",oFont07)
    oPrinter:Say(390,395,cCliente,oFont15)

    oPrinter:Say(420,215,"Lote",oFont17)
    oPrinter:Say(430,215,"(Lot)",oFont07)
    oPrinter:Say(420,395,cLote,oFont17)
    oPrinter:Code128(405,595,cLote, 1, 27)

    oPrinter:Say(455,215,"Lote Materia Prima",oFont17)
    oPrinter:Say(465,215,"(Raw Material Lot)",oFont07)
    oPrinter:Say(455,395,cLmatp,oFont17)
 
    oPrinter:Say(490,215,"Data da Embalagem",oFont17)
    oPrinter:Say(500,215,"(Date of Packaging)",oFont07)
    oPrinter:Say(490,395,"",oFont17)

    oPrinter:Say(525,215,"Peso Bruto",oFont17)
    oPrinter:Say(535,215,"(Gross Weight)",oFont07)
    oPrinter:Say(525,395,alltrim(transform(cPesobrt, "@E 999,999.99")),oFont17)

    //alltrim(transform(mv_par03,"@E 999,999.999"))
    oPrinter:Say(560,215,"Peso Líquido",oFont17)
    oPrinter:Say(570,215,"(Net Weight)",oFont07) 
    //nPEst := mv_par08 * mv_par03
    oPrinter:Say(560,395,alltrim(transform(mv_par10,"@E 999,999.99")) ,oFont17)

    //QrCode
    //cQCodMsa := alltrim(cCodMsa)+"$"
    //cQCodCli := alltrim(cCodCli)+"$"
    //cQQnt 	:= alltrim(str(mv_par03))+"$"
    //cQLote  := alltrim(cLote)+"$"
    //cQPbrt := alltrim(cPesobrt)+"$"
    //cQPliq := alltrim(cPesoliq)+"$"
    //cQrstr  := cQCodMsa+cQQnt+cQLote+cQPbrt //string qrcode full

    //QrCode
    //oPrinter:QRCode(375,27,cQrstr, 160)

/*     If bPecaA4
        if cImgTpya == "PNG"
            oPrinter:SayBitmap( 433,57 , "/SYSTEM/"+alltrim(mv_par02)+".png",,)
        Elseif cImgTpya == "JPG"
            oPrinter:SayBitmap( 433,57 , "/SYSTEM/"+alltrim(mv_par02)+".jpg",,)

        Endif
        
    Endif */

    oPrinter:EndPage()
    //oPrinter:Preview() - Para pdf preview, Para impressão direto impressora Print
    oPrinter:Print()

    

Return



////////////////////////////////////////////////////////////////////////////////////
// @Função: LxeTmp(cCodMP)                                                        //
// @Desc: Cria tabela temporaria para atualização dos saldos de Lote x Endereço   //
//durante a utilização do ACD.                                                    //
// @Retorno: Nome tabela temporaria.                                              //
////////////////////////////////////////////////////////////////////////////////////

Static Function LxeTmp(cCodMP,cPedido,cSolic)
    Local   nLoop,x    :=  0
    Private aLxeInit := {}

    cCodMP := PADR(cCodMP,TamSx3('B1_COD')[1])

    //Conjunto de soluções para manter consistência no estoque.
    //Adiciona o produto à array de produtos bloqueados.
    //TravaProd(cCodMP,cPedido,cSolic)

    //VTALERT("BP1")
    If Empty(cCodMP)
        VTALERT("Bug LXETMP. Codigo de produto invalido. SOLUÇÃO: Contate o administrador do sistema.")
        Return
    Endif
    
    cAliasName := "TMP_"+alltrim(cCodMP)

    /**Puxa estado Inicial Lote x Endereço**/
    IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF

    cQry := "SELECT DISTINCT BF_LOCAL,BF_PRODUTO AS B8_PRODUTO,BF_LOTECTL AS B8_LOTECTL,BF_LOCALIZ,BF_EMPENHO,BF_QUANT AS B8_SALDO FROM "+RETSQLNAME("SBF")+" AS SBF WHERE BF_PRODUTO = '"+cCodMP+"' AND BF_QUANT > 0 AND SBF.D_E_L_E_T_ = ' ' AND BF_LOCAL = '01'"
    TCQUERY cQry NEW ALIAS "TMP"
    TCSETFIELD("TMP","B8_DATA","D",8,0)

    DbSelectArea("TMP")
    TMP->(Dbgotop())

    While !TMP->(Eof())
        aAdd(aLxeInit,{TMP->BF_LOCAL,TMP->B8_PRODUTO,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,TMP->B8_SALDO-TMP->BF_EMPENHO})
        TMP->(DbSkip())
    EndDo 
    
    //Cria a temporária
    //Campos
    /* BF_LOCAL: C REAL 2
    B8_PRODUTO:C REAL 15
    B8_LOTE: C REAL 10
    BF_LOCALIZ: C REAL 15
    B8_SALDO: N REAL 14 2 */

    oTempTable := FWTemporaryTable():New(cAliasName)
    //vtalert("bp2")

    //Adiciona no array das colunas as que serão incluidas (Nome do Campo, Tipo do Campo, Tamanho, Decimais)
    aFields := {}
    aAdd(aFields, {"FILIAL", "C",TamSx3("B1_FILIAL")[1],0})
    aAdd(aFields, {"LOCALX",   "C", TamSx3('BF_LOCAL')[1], 0})
    aAdd(aFields, {"PRODUTO",   "C", TamSx3('B8_PRODUTO')[1], 0})
    aAdd(aFields, {"LOTE", "C", TamSx3('B8_LOTECTL')[1], 0})
    aAdd(aFields, {"LOCALIZ", "C", TamSx3('BF_LOCALIZ')[1], 0})
    aAdd(aFields, {"SALDO", "N", TamSx3('B8_SALDO')[1], 2})

 
    //Define as colunas usadas
    oTempTable:SetFields( aFields )

     //---------------------
    //Criação dos índices
    //---------------------
    oTempTable:AddIndex("01", {"FILIAL", "PRODUTO"} )

    //Efetua a criação da tabela
    oTempTable:Create()

     //------------------------------------
    //Pego o alias da tabela temporária
    //------------------------------------
    cAliasReal := oTempTable:GetAlias()

    //--------------------------------------------------------
    //Pego o nome real da tabela temporária no banco de dados
    //--------------------------------------------------------
    cTableName := oTempTable:GetRealName()
    

    //VTALERT("nomeTabela:"+cTablename)

    //VTALERT("Filial:"+FWFilial())

    //------------------------------
    //Inserção de dados para testes
    //------------------------------
    For x:=1 To len(aLxeInit)
        //LOCAL,PRODUTO,LOTE,END,SALDO
        //Padrão Alias: TMP_CODPROD
        (cAliasName)->(DBAppend())
        (cAliasName)->FILIAL  := FWFilial()
        (cAliasName)->LOCALX   := aLxeInit[x,1]
        (cAliasName)->PRODUTO := aLxeInit[x,2]
        (cAliasName)->LOTE    := aLxeInit[x,3]
        (cAliasName)->LOCALIZ := aLxeInit[x,4]
        (cAliasName)->SALDO   := aLxeInit[x,5]
        (cAliasName)->(DBCommit())
        //nPubMxEst
    End
        


    //Retorna nome da tabela, que será posteriormente adicionado a array.
    //Em momentos que houver necessidade de uso da tabela, será utilizado a aScan
    //para pegar a posição e nome tabela


Return cTableName




///////////////////////////////////////////////////////////////////////
//Função: LxeMxTmp()                                                 //
//Retorna o maximo disponível em estoque de acordo com o produto.    //
//Puxa da tabela temporária as informações.                          //
///////////////////////////////////////////////////////////////////////

Static Function LxEMxTmp(cCodMP)

    cAlias := "TMP_"+alltrim(cCodMP)
    nMxEtq := 0


    //Filial,Local,Produto,Lote,Localiz,Saldo
    //cQuerySQL := "SELECT FILIAL, LOCALX , PRODUTO , LOTE , LOCALIZ, SALDO FROM " + str(cTableName)
    //svtalert("passou cquery")
    //DBUseArea(.T., "TOPCONN", TCGenQry(,,cQuerySQL), cAlias, .T., .T.)
    (cAlias)->(DbGoTop())
    //vtalert("LxeMxTmp")

    while !(cAlias)->(Eof())
    //TMP->B8_SALDO-TMP->BF_EMPENHO-TMP->BF_XRESPL
        nSaldo := (cAlias)->SALDO
        nMxEtq += (cAlias)->SALDO
        (cAlias)->(DBSkip())
    enddo



Return nMxEtq


/*/ Função: ClsTmp()
    Fecha tabelas/alias temporários que estejam em aberto.
    @type  Function
    @author Arinus K. de Oliveira
    @since 31/05/2022
    @Desc Loopa entre todos os alias em aberto utilizando a array aTmpNms,
    que grava todos os produtos que possuem alias temporario em aberto.
    /*/
Static Function ClsTmp()
    Local y

    //aTmpNMS := {Produto,Tabela,Alias}
    For y:=1 To Len(aTmpNms)
        cAliasNt := aTmpNms[y,3]
        (cAliasNt)->(DbCloseArea())

    End
Return 


                            

/*/{Protheus.doc} chkProd(cProduto,cUser)
    Checa no semáforo se o produto está disponível para uso para o usuário que requisitou.
    @type  Function
    @author Arinus Kruszynski de Oliveira
    @since 18/07/2022
    @version 1
    @param param_name, param_type, param_descr
    @return .T. para disponivel e .F. para nao disponivel
    @see (links_or_references)
/*/

Static Function chkProd(cProduto,cUser)
    Private lRet
    //reset
    cUsrsprd := ""
    If Select("SZ9") > 0
        SZ9->(DbCloseArea())
    Endif

    //Se existir Produto onde o usuario seja diferente do que esta manipulando o ACD.
    cQry := "SELECT * FROM "+RETSQLNAME("SZ9")+" AS SZ9 WHERE Z9_CODMP = '"+cProduto+"' AND D_E_L_E_T_='' AND Z9_SOLIC != '"+cUser+"' "
    TCQUERY cQry NEW ALIAS "SZ9"
    DbSelectArea("SZ9")
    SZ9->(DbGoTop())
    While SZ9->(!Eof())
        If SZ9->Z9_SOLIC != cUser
            cUsrsprd := SZ9->Z9_SOLIC
            Return lRet := .F.
        Endif
        SZ9->(DbSkip())
    Enddo

    if empty(cUsrsprd)
        Return lRet := .T.
    Endif



Return lRet

/*/{Protheus.doc} chkPdV()
    Checa se o pedido está disponível para distribuição via ACD. (função semáforo)
    @type  Function
    @author Arinus Kruszynski de Oliveira
    @since 18/07/2022
    @version 1
    @param param_name, param_type, param_descr
    @return lRet, L, Retorna .T. para produto disponível e .F. para produto não disponível.
    @example
    (examples)
    @see (links_or_references)
    /*/
Static Function chkPdv(cPedido)
    Private lRet := .F.

    If Select("SZ9")
        SZ9->(DbCloseArea())
    Endif

    DbSelectArea("SZ9")
    SZ9->(DbSetOrder(2)) // 1-Filial+Pedido+Produto 2-Pedido   3-   4-
    If DbSeek(xFilial("SZ9")+cPedido)
        SZ9->(DbGoTop())
        //Pedido não disponível

        cUsrss := SZ9->Z9_SOLIC
        

        lRet := .F.
    Else
        //Pedido disponível
        lRet := .T.
    Endif

Return lRet

/*/{Protheus.doc} Trava Ped(cPedido,cSolic)
    Adiciona registro ao semáforo do picklist de saída ACD.
    @type  Function
    @author Arinus K.
    @since 18/07/2022
    @version 1
    @param Pedido,Produto,Solicitante
    @return lRet
/*/

//

Static Function TravaPed(cPedido,cSolic)
    Private lRet

    //SZ9 -> Adiciona registro ao semáforo do picklist de saída.
    DbSelectArea("SZ9")
    SZ9->(DbGoBottom())
    //SZ9->(DbGoTop())
    Reclock("SZ9",.T.)
    REPLACE Z9_FILIAL with xFilial("SZ9")
    REPLACE Z9_PEDIDO with cPedido
    REPLACE Z9_SOLIC with cSolic
    REPLACE Z9_DATA with Date()
    SZ9->(MsUnlock())
    aAdd(aRecnz9,{SZ9->(Recno())}) //salva recno para retirar os semáforos depois..
    SZ9->(DbCloseArea())

Return

/*/{Protheus.doc} TravaProd
    Trava produto SZ9
    @type  Function
    @author Arinus K
    @since 18/07/2022
    @version version
    @param param_name, param_type, param_descr
    @return return_var, return_type, return_description
    @example
    (examples)
    @see (links_or_references)
    /*/
Static Function TravaProd(cProd,cPedido,cSolic)

    //SZ9 -> Adiciona registro ao semáforo do picklist de saída.
    If Select("SZ9") > 0
        SZ9->(DbCloseArea())
    Endif
    
    DbSelectArea("SZ9")
    SZ9->(DbGoBottom())
    Reclock("SZ9",.T.)
    REPLACE Z9_FILIAL with xFilial("SZ9")
    REPLACE Z9_PEDIDO with cPedido
    //REPLACE Z9_CODMP with cProd
    REPLACE Z9_CODMP WITH cProd
    REPLACE Z9_SOLIC with cSolic
    REPLACE Z9_DATA with Date()
    SZ9->(MsUnlock())
    //aAdd(aRecnz9,{SZ9->(Recno())}) //Salva recno do reg semáforo.. Utilizado para retirar os semáforo posteriormente.
    SZ9->(DbCloseArea())

Return 



///////////////////////////////////////////////////////////////
//  Funçao que adiciona a reserva os itens adicionados no PL //
// A430Reserva+C6_XRESPL
//Função antiga
//////////////////////////////////////////////////////////////

Static Function AdToRes(aZ8Insrt,cNumsoli,cPdVenda)
    Local nlin
    Local aOPERACAO := {1,"NF","",UsrRetName(),xFilial(""),cNumsoli}
    Local cNUMERO   := ""
    Local cPRODUTO := ""
    Local cLOCAL    := "01"
    Local nQUANT    := 0
    Local aLOTE     := {"","","",""}   

    Private nTemp := 0.00
    //Private cLocal := "01" //armazem
    Private cProdx := "" //produto
    Private cLt   := "" //lote
    Private cEndc := "" //endereço
    Private cItPdv := "" //item do picklist teve origem em qual item do pedido de venda?
    Private lRes := .F.


    For nlin:=1 to len(aZ8Insrt)
        
        //A linha deve ser empenhada em todos os campos necessários, ou nada entra
        //como empenho e o Z8_QTDEMP fica como 0.
            cProdx   := aZ8Insrt[nlin,1]
            cLt      := aZ8Insrt[nlin,5]
            cEndc    := aZ8Insrt[nlin,6]
            cItPdv   := aZ8Insrt[nlin,12]

            //Vars A430Reserv
            cNUMERO  := GETSXENUM("SC0","C0_NUM") 
            cPRODUTO := cProdx
            nQUANT   := aZ8Insrt[nlin,7] 
            aLOTE    := {"",cLt,cEndc,""}

            If !A430Reserv(aOPERACAO,cNUMERO,cPRODUTO,cLOCAL,nQUANT,aLOTE) 
                VTALERT("Erro ao realizar reserva. Nenhum saldo sera empenhado.")   
                RollBackSx8()
                Return lRes := .F. 
            else
                cResNum := cNumero
                aAdd(aDocsRes,{cResNum})
                ConfirmSX8()
            Endif

            /*Reserva Sc6*/
            If Select("SC6")>0
                SC6->(DbCloseArea())
            Endif
            DbSelectArea("SC6")
            DbSetOrder(1)
            If DbSeek(xFilial()+cPdVenda+cItPdv+cProdx)
                RECLOCK("SC6",.F.)
                REPLACE C6_XRESPL with SC6->C6_XRESPL+aZ8Insrt[nlin,7]
                SC6->(MsUnlock())
            Else
                VTALERT("DbSeek SC6. Nenhum saldo será empenhado.")   
                Return lRes := .F. 
            Endif    

    Next nlin

    lRes := .T.
     
Return lRes




/////////////////////////////////////////////////////////////////
//  Adiciona reserva PL com A430Reserv(MATA430),rotina padrao //
//  Caso o saldo adicionado à reserva esteja indisponível,    //
//  emite mensagem de erro ao usuário e parte para proximas l //
////////////////////////////////////////////////////////////////

Static Function AdResLin(cProdx,cLote,cEnd,nQnt,cNumSoli,cItPdv,cPdVenda,cUsrsol,cItpls,cNatOp)
    Private nlin
    Private aOPERACAO := {1,"NF","",cUsrsol,xFilial(),cNumsoli}
    Private cNUMERO   := ""
    Private cPRODUTO := ""
    Private cLOCAL    := "01"
    Private nQUANT    := 0
    Private aLOTE     := {"","","",""} 
    Private lxA       := .F. 
    Private lxB       := .F.
    Private lRes      := .F.

    //Vars A430Reserv
    cNUMERO  := GETSXENUM("SC0","C0_NUM") 
    cPRODUTO := padr(alltrim(cProdx),TamSx3('B1_COD')[1])
    nQUANT   := nQnt
    aLOTE    := {"",cLote,cEnd,""}

    A430Reserv(aOPERACAO,cNUMERO,cPRODUTO,cLOCAL,nQUANT,aLOTE)

    If Select("SC0")>0
        SC0->(DbCloseArea())
    Endif

    DbSelectArea("SC0")
    DbSetOrder(1)
    //Se a reserva foi efetuada com sucesso..
    If DbSeek(xFilial("SC0")+cNumero+cProdx+cLocal)
        cResNum := cNumero
        cDoc := cResNum
        ConfirmSX8()
        lXa := .T.

    Else
        //Quanto erro ao realizar reserva
        //VTALERT("Erro ao realizar reserva. Nenhum saldo sera empenhado.")   
        RollBackSx8()
        Return lRes := .F. 
    Endif

    //Se venda..
    If cNatOp == "V"
        /*Reserva Sc6*/
        If Select("SC6")>0
            SC6->(DbCloseArea())
        Endif
        DbSelectArea("SC6")
        DbSetOrder(1)
        If DbSeek(xFilial()+cPdVenda+cItPdv+cProdx)
            RECLOCK("SC6",.F.)
            REPLACE C6_XRESPL with SC6->C6_XRESPL+nQnt
            SC6->(MsUnlock())
            
            lxB := .T.
        Else
            VTALERT("DbSeek SC6. Nenhum saldo será empenhado.")   
            Return lRes := .F. 
        Endif
       
        //se adicionou os dois
        If lxA .AND. lxB
            lRes := .T.
        else
            lRes := .F.
        Endif

    //Se beneficiamento..
    Elseif cNatOp == "B"
        If lxA
            lRes := .T.
        Else
            lRes := .F.
        Endif
    Endif

     
Return lRes

