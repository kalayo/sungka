board  = c( 7, 7, 7, 7, 7, 7, 7, 0,
	     	7, 7, 7, 7, 7, 7, 7, 0)			# Initialize board.

# Note that board approximates reality.
# 1   2   3   4   5   6   7   8 ---- Pepe's wells [1:7] and head at [8].
# 9  10  11  12  13  14  15  16 ---- Pilar's wells [9:15] and head at [16].

# Does not incorporate strategy of any kind, so Pepe and Pilar choose wells at random.

par(mfrow=c(2,2))							# Plot turns to visualize.
plot(board, ylim = c(-1, 60), pch = 15)

dropsp = rep(F, 16)							# Initialize vectors to follow drops.
dropsr = rep(F, 16)

if (sample(c(T,F), 1)) {					# Pepe and Pilar flip a coin to start.
	pepe = T									# Pepe and Pilar are the prototypical kids in...
	pilar = F									# ...Filipino kiddie literature.
	
	housep = sample(1:7, 1)					# Pepe starts if he wins flip, ...
	shellsp = board[housep]					# ...and picks up shells.
	board[housep] = 0
	shellsr = 0
	
} else {									# Pilar starts if she wins flip.
	pepe = F
	pilar = T
	houser = sample(9:15, 1)
	shellsr = board[houser]
	board[houser] = 0
	shellsp = 0
}

while (pepe | pilar) {						# Pepe and Pilar take turns according to sungka.

if (pilar) {									# Pilar drops shells if it's her turn...
while (shellsr > 0) {
while ((houser + 1) <= 16 & shellsr > 0) {
	if ((houser + 1) == 8) houser = houser + 1 else houser = houser		#...skipping only Pepe's head.
	dropsr[(houser+1)] = T						
	houser = houser + 1
	shellsr = shellsr - 1
	}	
stopr = houser
board = board + dropsr
dropsr = rep(F, 16)
houser = 0
}
lines(board, col = 'blue')					# Plot Pilar's turn.

# Sungka rules after Pilar drops her last shell.
if (stopr == 16) {							# If Pilar drops last shell into own head, ...
	if (sum(board[9:15]) > 0) {				# ... she continues if she has shells left.
		pepe = F
		pilar = T
		
		# Pilar must choose a non-empty well.
		if (length(which(board[9:15] > 0)) == 1) {houser = (which(board[9:15] > 0)) + 8} else { 
			houser = sample(c(which(board[9:15] > 0)), 1) + 8 }
		
		shellsr = board[houser]
		dropsr = rep(F, 16)
		board[houser] = 0
	
	} else {									# Pepe takes over if she has no shells left.
		if (sum(board[1:7]) > 0) {
			pilar = F
			pepe = T
			
			# Pepe must also choose a non-empty well.
			if (length(which(board[1:7] > 0)) == 1) housep = (which(board[1:7] > 0)) else {
				housep = sample((which(board[1:7] > 0)), 1) }
			
			shellsp = board[housep]
			dropsp = rep(F, 16)
			board[housep] = 0
		
		} else {							# Game ends if neither player has shells left.
			pilar = F
			pepe = F
		}
	}

} else {									# If Pilar instead drops into a well, ...
	if (board[stopr] > 1) {					# ... she continues if well is not empty.
		pepe = F
		pilar = T
		houser = stopr
		shellsr = board[stopr]
		dropsr = rep(F, 16)
		board[stopr] = 0
	
	} else {									# If well is empty, ...
		if (stopr < 8) {					# ... Pilar loses turn if it's not hers.
			pilar = F
			pepe = T
			
			# Pepe must choose a non-empty well.
			if (length(which(board[1:7] > 0)) == 1) housep = (which(board[1:7] > 0)) else {
				housep = sample((which(board[1:7] > 0)), 1) }
			
			shellsp = board[housep]
			dropsp = rep(F, 16)
			board[housep] = 0
			
		# But, if it's her own well, ...
		} else {							
			
			if ((sum(board[1:7]) - board[stopr - (stopr-8)*2]) > 0) {			# Pilar loses turn if ... 
				if (board[stopr - (stopr-8)*2] == 0) {		# ... Pepe's opposing well is empty ... 
					pilar = F								# ... and Pepe has shells left.
					pepe = T
					
					# As he must, Pepe chooses a non-empty well.
					if (length(which(board[1:7] > 0)) == 1) housep = (which(board[1:7] > 0)) else { 
						housep = sample((which(board[1:7] > 0)), 1) }
					
					shellsp = board[housep]
					dropsp = rep(F, 16)
					board[housep] = 0
				
				# Pilar also loses turn, but captures if Pepe's opposing well is not empty.
				} else {
					pilar = F
					board[16] = board[16] + board[stopr - (stopr-8)*2] + 1
					board[stopr - (stopr-8)*2] = 0
					board[stopr] = 0
					pepe = T
					
					# Pepe's choices are limited to non-empty wells.
					if (length(which(board[1:7] > 0)) == 1) housep = (which(board[1:7] > 0)) else {
						housep = sample((which(board[1:7] > 0)), 1) }
					
					shellsp = board[housep]
					dropsp = rep(F, 16)
					board[housep] = 0
				}
				
			} else {						# Pilar also continues because Pepe has no shells left.
				pepe = F
				pilar = T
				houser = stopr
				shellsr = board[houser] 
				dropsr = rep(F, 16)
				board[houser] = 0
			}
		}
	}
}


} else {									# Pepe drops shells if its his turn, ...
while (shellsp > 0) {							# ... abiding by the same rules, and ...
while ((housep+1) < 16 & shellsp > 0) {			# ... skipping only Pilar's head.
	dropsp[(housep+1)] = T						
	housep = housep + 1
	shellsp = shellsp - 1
	}
stopp = housep
board = board + dropsp
dropsp = rep(F, 16)
housep = 0
}
lines(board, col = 'orange')

if (stopp == 8) {
	if (sum(board[1:7]) > 0) {
		pilar = F
		pepe = T
		
		if (length(which(board[1:7] > 0)) == 1) housep = (which(board[1:7] > 0)) else { 
			housep = sample((which(board[1:7] > 0)), 1) }
		
		shellsp = board[housep]
		dropsp = rep(F, 16)
		board[housep] = 0
		
	} else {
		if (sum(board[9:15]) > 0) {
			pepe = F
			pilar = T
			
			if (length(which(board[9:15] > 0)) == 1) {houser = (which(board[9:15] > 0)) + 8} else { 
				houser = sample(c(which(board[9:15] > 0)), 1) + 8 }
			
			shellsr = board[houser]
			dropsr = rep(F, 16)
			board[houser] = 0
		
		} else {
			pilar = F
			pepe = F
		}
	}

} else {
	if (board[stopp] > 1) {
		pilar = F
		pepe = T
		housep = stopp
		shellsp = board[stopp]
		dropsp = rep(F, 16)
		board[stopp] = 0
	
	} else {
		if (stopp > 8) {
			pepe = F
			pilar = T
			
			if (length(which(board[9:15] > 0)) == 1) {houser = (which(board[9:15] > 0)) + 8} else { 
				houser = sample(c(which(board[9:15] > 0)), 1) + 8 }
			
			shellsr = board[houser]
			dropsr = rep(F, 16)
			board[houser] = 0
		
		} else {
			if ((sum(board[9:15])- board[(8-stopp)*2 + stopp]) > 0) { 
				if (board[(8-stopp)*2 + stopp] == 0) {
					pepe = F
					pilar = T
					
					if (length(which(board[9:15] > 0)) == 1) {houser = (which(board[9:15] > 0)) + 8} else 						{ houser = sample(c(which(board[9:15] > 0)), 1) + 8 }
					
					shellsr = board[houser]
					dropsr = rep(F, 16)
					board[houser] = 0
				
				} else {
					pepe = F
					board[8] = board[8] + board[(8-stopp)*2 + stopp] + 1
					board[(8-stopp)*2 + stopp] = 0
					board[stopp] = 0 
					pilar = T
					
					if (length(which(board[9:15] > 0)) == 1) {houser = (which(board[9:15] > 0)) + 8} else 						{ houser = sample(c(which(board[9:15] > 0)), 1) + 8 }
					
					shellsr = board[houser]
					dropsr = rep(F, 16)
					board[houser] = 0
				}
			
			} else {
				pilar = F
				pepe = T
				housep = stopp
				shellsp = board[housep]
				dropsp = rep(F, 16)
				board[housep] = 0
			}
		}
	}
}
}
print(board)								# Print board as game progresses.
}