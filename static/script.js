let disable = false; //disable multiple clicks
async function handleClick(cellid,formid){
    
    let currentTurn = document.getElementById("turn").innerHTML;

    // only allow clicks when it is your turn
    if(( (currentTurn.includes("user") && cellid.includes("cpu")) || (currentTurn.includes("cpu") && cellid.includes("user")) ) && !disable){

    

      document.getElementById(cellid).style.backgroundColor = "red";
      
      document.getElementById(formid).submit();
      disable=true;
    }


}

function loadColors(){
    let userBoardStatus = document.getElementById("user-board-status").innerHTML;
    let cpuBoardStatus = document.getElementById("cpu-board-status").innerHTML;
    for(let i=0;i<100;i++){
        
        if(userBoardStatus[i] == 1){
          document.getElementById("usercell" + i).style.backgroundColor = "red";
        }
        if(cpuBoardStatus[i] == 1){
          document.getElementById("cpucell" + i).style.backgroundColor = "red";
        }
      
    }

}
  loadColors(); // we want to call this every time we load a page