let disable = false; //disable multiple clicks
let bombSet = false; // whether the user clicked the "Bomb" button or not
function handleClick(cellid,formid,gameOver){
    // only allow clicks when it is your turn, and make sure a form exists in order for click to occur
    if(!disable && document.getElementById(formid) && !document.getElementById("cpu-turn") && !gameOver){
      document.getElementById(cellid).style.backgroundColor = "red";
      if (bombSet){
        document.getElementById(formid.replace("form","input")).value = "bomb" + document.getElementById(formid.replace("form","input")).value;
      }
      document.getElementById(formid).submit();
      disable=true;
    }
}



function loadSinglePlayer(userBoardStatus, cpuBoardStatus, turn, gameOver){

    if(turn === "cpu"){
      document.getElementById("user-board").style.color = "darkgreen"; 
       // when CPU turn, automatically send a request to the /cpu_turn endpoint
      if(!gameOver){
        document.getElementById("cpu-turn").submit();
      }
    }
    else{
      document.getElementById("cpu-board").style.color = "darkgreen"; 
    }



    
    for(let i=0;i<100;i++){
        
        // miss
        if(userBoardStatus[i] == 1){
          document.getElementById("usercell" + i).innerHTML = "•";
        }
        // ship
        else if(userBoardStatus[i] == 2){
          document.getElementById("usercell" + i).style.backgroundColor = "blue";
        }
        // hit
        else if(userBoardStatus[i] == 3){
          document.getElementById("usercell" + i).innerHTML = "X";
          document.getElementById("usercell" + i).style.color = "red";
          document.getElementById("usercell" + i).style.borderColor = "red";
        }
        // sunk
        else if(userBoardStatus[i]==4){
          document.getElementById("usercell" + i).style.backgroundColor = "red";
          document.getElementById("usercell" + i).style.color = "red";
          document.getElementById("usercell" + i).style.borderColor = "red";
        } 
        // mine
        else if(userBoardStatus[i]==5){
          document.getElementById("usercell" + i).style.backgroundColor = "gray";
        } 
        // mine hit
        else if(userBoardStatus[i]==6){
          document.getElementById("usercell" + i).style.backgroundColor = "black";
          document.getElementById("usercell" + i).innerHTML = "";
        } 
        // miss
        if(cpuBoardStatus[i] == 1){
          document.getElementById("cpucell" + i).innerHTML= "•";
        }
        // only reveal CPU ships after game is over
        else if(cpuBoardStatus[i] == 2 && gameOver ){
          document.getElementById("cpucell" + i).style.backgroundColor = "blue";
        }
        // hit
        else if(cpuBoardStatus[i] == 3){
          
          document.getElementById("cpucell" + i).innerHTML = "X";
          document.getElementById("cpucell" + i).style.color = "red";
          document.getElementById("cpucell" + i).style.borderColor = "red";
        }
        // sunk
        else if(cpuBoardStatus[i]==4){
          document.getElementById("cpucell" + i).style.backgroundColor = "red";
          document.getElementById("cpucell" + i).style.color = "red";
          document.getElementById("cpucell" + i).style.borderColor = "red";
        } 
        // only reveal CPU mine after game is over
        else if(cpuBoardStatus[i]==5 && gameOver ){
          document.getElementById("cpucell" + i).style.backgroundColor = "gray";
        } 
        // mine hit
        else if(cpuBoardStatus[i]==6){
          document.getElementById("cpucell" + i).style.backgroundColor = "black";
          document.getElementById("cpucell" + i).innerHTML = "";
        } 
      
    }

}