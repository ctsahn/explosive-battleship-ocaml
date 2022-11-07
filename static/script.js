let disable = false;
async function handleClick(cellid,formid){
    //disable multiple clicks
    if(disable == false ){
      document.getElementById(cellid).style.backgroundColor = "red";
      
      document.getElementById(formid).submit();
      disable=true;
    }

}

function loadColors(){
    let boardStatus = document.getElementById("opponent-board-status").innerHTML;

    for(let i=0;i<100;i++){
        
        if(boardStatus[i] == 1){
        document.getElementById("cell" + i).style.backgroundColor = "red";
        }
      
    }

}
  loadColors(); // we want to call this every time we load a page