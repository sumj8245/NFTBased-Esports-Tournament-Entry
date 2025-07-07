# NFT-Based Esports Tournament Smart Contract

This Clarity smart contract enables the creation and management of esports tournaments with NFT-based entry requirements and automated prize distribution.

## Features

- Create tournaments with customizable parameters
- Optional NFT ownership requirement for participation
- Registration with entry fees that build the prize pool
- Tournament lifecycle management (creation, registration, closing, ending)
- Winner designation with flexible prize distribution
- Prize claiming by winners
- Remaining funds withdrawal by contract owner

## Contract Functions

### Tournament Management

- `create-tournament`: Create a new tournament with name, description, participant limit, entry fee, and optional NFT requirement
- `close-tournament-registration`: Close registration for a tournament
- `end-tournament`: Mark a tournament as completed
- `set-tournament-winners`: Set the winners and prize distribution for a tournament
- `withdraw-remaining-funds`: Allow the contract owner to withdraw any remaining funds

### Participant Functions

- `register-for-tournament`: Register for a tournament by paying the entry fee
- `claim-prize`: Claim prize money after being designated as a winner

### NFT Management

- `register-nft`: Register NFT ownership for a user (admin function)

### Read-Only Functions

- `get-tournament-counter`: Get the total number of tournaments created
- `get-tournament`: Get details about a specific tournament
- `get-participant-status`: Check a participant's status in a tournament
- `get-tournament-winner`: Get information about a winner at a specific position
- `check-user-nft`: Check if a user owns a specific NFT

## Usage Example

1. Contract owner creates a tournament:
```clarity
(contract-call? .esport create-tournament "Stacks Gaming Championship" "The ultimate gaming showdown on Stacks" u64 u10000000 none)
```

2. Players register for the tournament:
```clarity
(contract-call? .esport register-for-tournament u1)
```

3. After registration period, owner closes registration:
```clarity
(contract-call? .esport close-tournament-registration u1)
```

4. After tournament completion, owner ends the tournament:
```clarity
(contract-call? .esport end-tournament u1)
```

5. Owner sets tournament winners and prize distribution:
```clarity
(contract-call? .esport set-tournament-winners u1 (list 
  {position: u1, winner: 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5, prize-amount: u50000000}
  {position: u2, winner: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG, prize-amount: u30000000}
  {position: u3, winner: 'ST2JHG361ZXG51QTKY2NQCVBPPRRE2KZB1HR05NNC, prize-amount: u20000000}
))
```

6. Winners claim their prizes:
```clarity
(contract-call? .esport claim-prize u1)
```

## Security Considerations

- Only the contract owner can create tournaments, set winners, and withdraw funds
- Participants can only register if the tournament is open and not full
- Winners can only claim their prizes once
- NFT ownership verification ensures eligibility for restricted 