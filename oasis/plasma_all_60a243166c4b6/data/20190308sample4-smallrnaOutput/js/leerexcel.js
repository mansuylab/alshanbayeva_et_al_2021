// JavaScript Document

function LeerExcel(){
        var IEx=document.all?1:0;//is IE      
        var ObjetoXLS;//Objeto excel
        var limiteDeBusqueda= 0
        var DatosExcel = "";
             
         /*Si sabemos cuantas columnas necesitamos leer, declaramos nuestras variables*/
        var columna1;
        var columna2 
        var columna3  

        if (IEx==1){
            if($('#InputExcel').val() != ""){
              /*Vaciamos la informacion de nuestro label*/
              $('#DatosDeMiExcel').html("");
                try{            
                    ObjetoXLS = new ActiveXObject("Excel.Application");      
                    ObjetoXLS.Workbooks.OPEN($('#InputExcel').val(),false,false);

                    /*Ciclo que nos ayudara a recorrer los renglones de mi excel*/
                    for (index = 1; ;index++){
                        /*Renglon en el que empezaremos a leer la informacion*/
                        columna1= ObjetoXLS.ActiveSheet.Cells(index,1).Value;
                        columna2= ObjetoXLS.ActiveSheet.Cells(index,2).Value;
                        columna3= ObjetoXLS.ActiveSheet.Cells(index,3).Value;
                                             
                         /*Si pasamos por mas de 12 renglones sin datos dejamos de seguir leyendo el excel*/            
                            if (columna1== "" && columna2== "" && columna3== "")
                            {
                                limiteDeBusqueda++;
                                if (limiteDeBusqueda> 12) break;
                            }
                            else
                            {
                                DatosExcel += columna1+'<br/>'+ columna1+'<br/>'+ columna1
                            }
                     
                    }              
                    ObjetoXLS.Application.Quit();
                   
                     /*Muestra datos leidos en el excel en nuestro label*/
                     $('#DatosDeMiExcel').html(DatosExcel);
                   
                }catch (e){
                    alert ('El archivo no se puede leer por cuestiones de seguridad');
                    $('#DatosDeMiExcel').html("");
                }  
            }else{
               $('#DatosDeMiExcel').html("");
            }
        }
           
}