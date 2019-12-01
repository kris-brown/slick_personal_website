
  // -Abrir popup a los que usan navegadores no compatibles con los estándares de Internet

  //  20 Agosto 2004, rehecho el 6.m8.2009 con jQuery.   Daniel Clemente.
  //  Dominio público.                         http://www.danielclemente.com

  // Para usarlo pon <div id="aviso-ie" />. Para generar el contenido del popup mira http://www.danielclemente.com/navega/ponlo.html

var ua=navigator.userAgent;


/* en 2012 veo que tras 8 años, está un poco desactualizado, y no le quiero hacer tanta publicidad como hasta ahora. Que lo vea quien quiera.
Los fundamentos siguen siendo ciertos (IE no cumple estándares, los cumple a su manera) pero menos, pues ahora con la competencia Microsoft se apresura.
Toca pasar a explicar otros motivos por los que IE es malo. Sobre todo, porque no da 4 libertades básicas a los usuarios/programadores; las de la GPL. Y en un navegador importa.

Pues eso, comento todo. */

/*


// Notas:
// - Opera a veces se identifica como IE
// - IE para MacOS 9 no tiene tantos fallos, y no hay tantas alternativas
var es_IE=ua.indexOf("MSIE")!=-1 && ua.indexOf("Opera")==-1 && ua.indexOf("Mac") == -1 ;
//es_IE=1; //probando

var cadena_HTML="<br><br><div style='border: 5px double blue; " +
 "margin: 30px; padding: 20px; background: #f0f000; font-size: 23px; " +
 " font-weight: bold; color: red;'>" +
 "&nbsp;&nbsp;&nbsp;<u>Aviso</u>:" +
 " usas Internet Explorer (uno de los peores navegadores)," +
 " y eso perjudica a Internet. " +
 "<a href='../navega/popup.html'>Te recomiendo otros</a>," +
 " para que puedas ver las webs c&oacute;modamente y sin errores.</div><br><br><br><br>";


$(document).ready(function(){

	if( es_IE ) {
		window.open('../navega/popup.html','aviso','status=yes,scrollbars=yes,resizable=yes');
		$("div#aviso-ie").append(cadena_HTML);
	}

});

*/
