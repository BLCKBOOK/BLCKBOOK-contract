import smartpy as sp

class AuctionErrorMessage:
    PREFIX = "AUC_"
    ID_ALREADY_IN_USE = "{}ID_ALREADY_IN_USE".format(PREFIX)
    UPLOADER_CANNOT_BID = "{}UPLOADER_CANNOT_BID".format(PREFIX)
    BID_AMOUNT_TOO_LOW = "{}BID_AMOUNT_TOO_LOW".format(PREFIX)
    AUCTION_IS_OVER = "{}AUCTION_IS_OVER".format(PREFIX)
    AUCTION_IS_ONGOING = "{}AUCTION_IS_ONGOING".format(PREFIX)
    SENDER_NOT_BIDDER = "{}SENDER_NOT_BIDDER".format(PREFIX)
    END_DATE_TOO_SOON = "{}END_DATE_TOO_SOON".format(PREFIX)
    END_DATE_TOO_LATE = "{}END_DATE_TOO_LATE".format(PREFIX)
    NOT_ADMIN = "{}NOT_ADMIN".format(PREFIX)
    CAN_NOT_CREATE_AN_AUCTION_TWICE = "{}CAN_NOT_CREATE_AN_AUCTION_TWICE".format(PREFIX)
    AUCTION_ID_SHOULD_BE_CONSECUTIVE = "{}AUCTION_ID_SHOULD_BE_CONSECUTIVE".format(PREFIX)
    NOT_100 = "{}SHARES_MUST_SUM_UP_TO_100".format(PREFIX)
    AUCTION_DOES_NOT_EXIST = "{}DOES_NOT_EXIST".format(PREFIX)

INITIAL_BID = sp.mutez(900000)
MINIMAL_BID = sp.mutez(100000)
INITIAL_AUCTION_DURATION = sp.int(24*5) # 5 days
MINIMAL_AUCTION_DURATION = sp.int(1) # 1 hour
MAXIMAL_AUCTION_DURATION = sp.int(24*14) # 14 days
AUCTION_EXTENSION_THRESHOLD = sp.int(60*5) # 5 minutes. Check whether we actually want this
BID_STEP_THRESHOLD = sp.mutez(100000)

class Contract(sp.Contract):
    def __init__(self, administrator):
        sp.set_type_expr(administrator, sp.TAddress)

        list_of_views = [
            self.get_balance
            , self.does_token_exist
            , self.count_tokens
            , self.all_tokens
            , self.is_operator
        ]

        metadata = {
            "name": "BLCKBOOK",
            "description": "BLCKBOOK beta implementation. Uses the didactic reference implementation of FA2,"
                + " a.k.a. TZIP-012, using SmartPy.\n\n",
            "version": "FA2",
            "views": list_of_views,
            "interfaces": ["TZIP-012", "TZIP-016"],
            "authors": ["Niels Hanselmann"], 
            "homepage": "https://blckbook.vote",
            "source": {"tools": ["SmartPy"], "location": "https://github.com/BLCKBOOK/BLCKBOOK-contract"},
            "permissions": {
                "operator": "owner-or-operator-transfer",
                "receiver": "owner-no-hook",
                "sender": "owner-no-hook"
            },
        }

        # Helper method that builds the metadata and produces the JSON representation as an artifact.
        self.init_metadata("BLCKBOOK-FA2", metadata) #the string is just for the output of the online-IDE

        self.init_type(sp.TRecord(
                administrator = sp.TAddress, 
                all_tokens = sp.TNat, 
                ledger = sp.TBigMap(sp.TPair(sp.TAddress, sp.TNat), sp.TRecord(balance = sp.TNat).layout("balance")),
                metadata = sp.TBigMap(sp.TString, sp.TBytes),
                operators = sp.TBigMap(sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat)
                .layout(("owner", ("operator", "token_id"))), sp.TUnit),
                paused = sp.TBool,
                token_metadata = sp.TBigMap(sp.TNat, sp.TRecord(token_id = sp.TNat, token_info = sp.TMap(sp.TString, sp.TBytes))
                .layout(("token_id", "token_info")))
            )
            .layout((("administrator", ("all_tokens", ("ledger", ("metadata", ("operators", ("paused", "token_metadata")))))))))
        self.init(
            administrator = administrator,
            all_tokens = 0,
            ledger = sp.big_map(tkey = sp.TPair(sp.TAddress, sp.TNat), tvalue = sp.TRecord(balance = sp.TNat)),
            metadata = sp.big_map(tkey = sp.TString, tvalue = sp.TBytes),
            operators = sp.big_map(tkey = sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat), tvalue = sp.TUnit),
            paused = False,
            token_metadata = sp.big_map(
                tkey = sp.TNat,
                tvalue = sp.TRecord(token_id = sp.TNat, token_info = sp.TMap(sp.TString, sp.TBytes))
            )
        )

    @sp.entry_point
    def balance_of(self, params):
        sp.verify(~ self.data.paused, 'FA2_PAUSED')
        sp.set_type(params, sp.TRecord(callback = sp.TContract(sp.TList(sp.TRecord(balance = sp.TNat, request = sp.TRecord(owner = sp.TAddress, token_id = sp.TNat)
        .layout(("owner", "token_id"))).layout(("request", "balance")))), 
        requests = sp.TList(sp.TRecord(owner = sp.TAddress, token_id = sp.TNat).layout(("owner", "token_id")))).layout(("requests", "callback")))
        def f_x0(_x0):
            sp.verify(self.data.token_metadata.contains(_x0.token_id), 'FA2_TOKEN_UNDEFINED')
            sp.if self.data.ledger.contains((sp.set_type_expr(_x0.owner, sp.TAddress), sp.set_type_expr(_x0.token_id, sp.TNat))):
                sp.result(sp.record(request = sp.record(owner = sp.set_type_expr(_x0.owner, sp.TAddress), token_id = 
                    sp.set_type_expr(_x0.token_id, sp.TNat)), balance = self.data.ledger[(sp.set_type_expr(_x0.owner, sp.TAddress), sp.set_type_expr(_x0.token_id, sp.TNat))].balance))
            sp.else:
                sp.result(sp.record(request = sp.record(owner = sp.set_type_expr(_x0.owner, sp.TAddress), token_id = 
                    sp.set_type_expr(_x0.token_id, sp.TNat)), balance = 0))
        responses = sp.local("responses", params.requests.map(sp.build_lambda(f_x0)))
        sp.transfer(responses.value, sp.tez(0), sp.set_type_expr(params.callback, 
        sp.TContract(sp.TList(sp.TRecord(balance = sp.TNat, request = sp.TRecord(owner = sp.TAddress, token_id = sp.TNat)
        .layout(("owner", "token_id"))).layout(("request", "balance"))))))

    @sp.entry_point
    def mint(self, params):
        sp.verify(sp.sender == self.data.administrator, 'FA2_NOT_ADMIN')
        sp.verify(params.amount == 1, 'NFT-asset: amount <> 1')
        sp.verify(~ (params.token_id < self.data.all_tokens), 'NFT-asset: cannot mint twice same token')
        self.data.ledger[(sp.set_type_expr(params.address, sp.TAddress), 
            sp.set_type_expr(params.token_id, sp.TNat))] = sp.record(balance = params.amount)
        sp.if ~ (params.token_id < self.data.all_tokens):
            sp.verify(self.data.all_tokens == params.token_id, 'Token-IDs should be consecutive')
            self.data.all_tokens = params.token_id + 1
            self.data.token_metadata[params.token_id] = sp.record(token_id = params.token_id, token_info = params.metadata)

    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, 'FA2_NOT_ADMIN')
        self.data.administrator = params

    @sp.entry_point
    def set_metadata(self, params):
        sp.verify(sp.sender == self.data.administrator, 'FA2_NOT_ADMIN')
        self.data.metadata[params.k] = params.v

    @sp.entry_point
    def set_pause(self, params):
        sp.verify(sp.sender == self.data.administrator, 'FA2_NOT_ADMIN')
        self.data.paused = params

    @sp.entry_point
    def transfer(self, params):
        sp.verify(~ self.data.paused, 'FA2_PAUSED')
        sp.set_type(params, BatchTransfer.get_type())
        sp.for transfer in params:
            sp.for tx in transfer.txs:
                sp.verify(((sp.sender == self.data.administrator) | (transfer.from_ == sp.sender)) | (self.data.operators.contains(sp.set_type_expr(sp.record(owner = transfer.from_, operator = sp.sender, token_id = tx.token_id), sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat).layout(("owner", ("operator", "token_id")))))), 'FA2_NOT_OPERATOR')
                sp.verify(self.data.token_metadata.contains(tx.token_id), 'FA2_TOKEN_UNDEFINED')
                sp.if tx.amount > 0:
                    sp.verify(self.data.ledger[(sp.set_type_expr(transfer.from_, sp.TAddress), sp.set_type_expr(tx.token_id, sp.TNat))]
                    .balance >= tx.amount, 'FA2_INSUFFICIENT_BALANCE')
                    self.data.ledger[(sp.set_type_expr(transfer.from_, sp.TAddress),
                        sp.set_type_expr(tx.token_id, sp.TNat))].balance = sp.as_nat(self.data.ledger[
                        (sp.set_type_expr(transfer.from_, sp.TAddress), sp.set_type_expr(tx.token_id, sp.TNat))].balance - tx.amount)
                    sp.if self.data.ledger.contains((sp.set_type_expr(tx.to_, sp.TAddress), sp.set_type_expr(tx.token_id, sp.TNat))):
                        self.data.ledger[(sp.set_type_expr(tx.to_, sp.TAddress), sp.set_type_expr(tx.token_id, sp.TNat))].balance += tx.amount
                    sp.else:
                        self.data.ledger[(sp.set_type_expr(tx.to_, sp.TAddress), 
                            sp.set_type_expr(tx.token_id, sp.TNat))] = sp.record(balance = tx.amount)

    @sp.entry_point
    def update_operators(self, params):
        sp.set_type(params, sp.TList(sp.TVariant(add_operator = sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat)
        .layout(("owner", ("operator", "token_id"))), remove_operator = sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat)
        .layout(("owner", ("operator", "token_id")))).layout(("add_operator", "remove_operator"))))
        sp.for update in params:
            with update.match_cases() as arg:
                with arg.match('add_operator') as add_operator:
                    sp.verify((add_operator.owner == sp.sender) | (sp.sender == self.data.administrator), 'FA2_NOT_ADMIN_OR_OPERATOR')
                    self.data.operators[sp.set_type_expr(sp.record(owner = add_operator.owner, operator = add_operator.operator, token_id = 
                    add_operator.token_id), sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat)
                    .layout(("owner", ("operator", "token_id"))))] = sp.unit
                with arg.match('remove_operator') as remove_operator:
                    sp.verify((remove_operator.owner == sp.sender) | (sp.sender == self.data.administrator), 'FA2_NOT_ADMIN_OR_OPERATOR')
                    del self.data.operators[sp.set_type_expr(sp.record(owner = remove_operator.owner, operator = 
                    remove_operator.operator, token_id = remove_operator.token_id), 
                    sp.TRecord(operator = sp.TAddress, owner = sp.TAddress, token_id = sp.TNat).layout(("owner", ("operator", "token_id"))))]

    @sp.entry_point
    def burn(self, address, token_id):
        """
            Burn tokens (destroy existing tokens)
            Args:
                address     : sp.TAddress - Token holder address
                token_id    : sp.TNat     - Id of the token
        """
        # We don't check for pauseness because we're the admin.
        sp.verify(sp.sender == self.data.administrator, 'FA2_NOT_ADMIN')
        sp.verify(self.data.token_metadata.contains(token_id), 'FA2_TOKEN_UNDEFINED')
        user = (sp.set_type_expr(address, sp.TAddress), sp.set_type_expr(token_id, sp.TNat))
        sp.verify(self.data.ledger.contains(user), 'FA2_WRONG_ADDRESS_FOR_BURN')
        sp.verify(self.data.ledger[user].balance == sp.nat(1), 'FA2_ADDRESS_DOES_NOT_HAVE_TOKEN_FOR_BURN')
        self.data.ledger[user].balance = sp.nat(0)

    @sp.offchain_view(pure = True)
    def get_balance(self, req):
        """This is the `get_balance` view defined in TZIP-12."""
        sp.set_type(
            req, sp.TRecord(
                owner = sp.TAddress,
                token_id = sp.TNat
            ).layout(("owner", "token_id")))
        user = sp.set_type_expr(req.owner, sp.TAddress)
        token = sp.set_type_expr(req.token_id, sp.TNat)
        ledger_key = sp.pair(user, token)
        sp.verify(self.data.token_metadata.contains(req.token_id), message = 'FA2_TOKEN_UNDEFINED')
        sp.result(self.data.ledger[ledger_key].balance)

    @sp.offchain_view(pure = True)
    def count_tokens(self):
        """Get how many tokens are in this FA2 contract."""
        sp.result(self.data.all_tokens)

    @sp.offchain_view(pure = True)
    def does_token_exist(self, tok):
        "Ask whether a token ID is exists."
        sp.set_type(tok, sp.TNat)
        sp.result(self.data.token_metadata.contains(tok))

    @sp.offchain_view(pure = True)
    def all_tokens(self):
        sp.result(sp.range(0, self.data.all_tokens))

    @sp.offchain_view(pure = True)
    def is_operator(self, query):
        sp.set_type(query,
                    sp.TRecord(token_id = sp.TNat,
                               owner = sp.TAddress,
                               operator = sp.TAddress).layout(
                                   ("owner", ("operator", "token_id"))))
        sp.result(
            self.data.operators.contains(sp.record(owner = query.owner,
                                        operator = query.operator,
                                        token_id = query.token_id))
        )

class AuctionCreateRequest():
    """ 
    The data-type class for creating a new auction
    """
    def get_type():
        return sp.TRecord(
        auction_id=sp.TNat,
        token_id=sp.TNat, 
        end_timestamp=sp.TTimestamp,  
        voter_amount=sp.TNat,
        uploader=sp.TAddress,
        bid_amount=sp.TMutez,
        ).layout(("auction_id",("token_id",("end_timestamp",("voter_amount",("uploader","bid_amount"))))))

class Auction():
    """ 
    The data-type class for a single auction contained in the auction-house-contract
    """
    def get_type():
        return sp.TRecord(
            token_id=sp.TNat, 
            end_timestamp=sp.TTimestamp,
            voter_amount=sp.TNat,
            uploader=sp.TAddress,
            bid_amount=sp.TMutez, #holds the current bid (at the start the minimal bid)
            bidder=sp.TAddress,
        ).layout(("token_id",("end_timestamp",("voter_amount",("uploader",("bid_amount","bidder"))))))


class AuctionHouse(sp.Contract):
    """ 
    The smart contract for the actual Auction-House
    """
    def __init__(self, administrator, blckbook_collector, money_pool, token_address):
        sp.set_type_expr(administrator, sp.TAddress)
        sp.set_type_expr(blckbook_collector, sp.TAddress)
        sp.set_type_expr(money_pool, sp.TAddress)
        sp.set_type_expr(token_address, sp.TAddress)
        self.init_type(sp.TRecord(
                administrator = sp.TAddress,
                blckbook_collector = sp.TAddress,
                money_pool = sp.TAddress,
                token_address = sp.TAddress,
                blckbook_share=sp.TNat,
                uploader_share=sp.TNat,
                voter_share=sp.TNat,
                auctions = sp.TBigMap(sp.TNat, Auction.get_type()),
                all_auctions = sp.TNat,
        ).layout(("administrator", ("blckbook_collector", ("money_pool", ("token_address", ("blckbook_share", ("uploader_share", ("voter_share", ("all_auctions", "auctions"))))))))))

        self.init(blckbook_share = sp.nat(25),
                    voter_share = sp.nat(15),
                    uploader_share = sp.nat(60),
                    auctions=sp.big_map(tkey=sp.TNat, tvalue = Auction.get_type()),
                    blckbook_collector = blckbook_collector,
                    administrator = administrator,
                    token_address = token_address,
                    money_pool = money_pool,
                    all_auctions= sp.nat(0))
     

    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.administrator = params

    @sp.entry_point
    def set_token_address(self, params):
        """ 
        Entry-Point for setting the FA2-Contract Address
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.token_address = params

    @sp.entry_point
    def set_blckbook_collector(self, params):
        """ 
        Entry-Point for setting the address of the blckbook_collector which will get the blckbook share of the auction-prices
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.blckbook_collector = params
    
    @sp.entry_point
    def set_money_pool_address(self, params):
        """ 
        Entry-Point for setting the address of the money_pool which will get the shares for the voters and will get called with the info how much every voter gets
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.money_pool = params

    @sp.entry_point
    def set_shares(self, blckbook_share, uploader_share, voter_share):
        """ 
        Entry-Point for setting the share percentages of the auction-price
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        sp.set_type_expr(blckbook_share, sp.TNat)
        sp.set_type_expr(uploader_share, sp.TNat)
        sp.set_type_expr(voter_share, sp.TNat)
        sp.verify(blckbook_share + uploader_share + voter_share == sp.nat(100), AuctionErrorMessage.NOT_100)
        self.data.blckbook_share = blckbook_share
        self.data.uploader_share = uploader_share
        self.data.voter_share = voter_share

    @sp.entry_point
    def create_auction(self, create_auction_request):
        """ 
        Entry-Point for creating a new auction
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN) # only admin can create auction (nft needs to be minted for auction-contract)
        sp.set_type_expr(create_auction_request, AuctionCreateRequest.get_type())

        sp.verify(~(create_auction_request.auction_id < self.data.all_auctions), message=AuctionErrorMessage.CAN_NOT_CREATE_AN_AUCTION_TWICE)
        sp.verify(self.data.all_auctions == create_auction_request.auction_id, message=AuctionErrorMessage.AUCTION_ID_SHOULD_BE_CONSECUTIVE)

        sp.verify(create_auction_request.end_timestamp  >= sp.now.add_hours(MINIMAL_AUCTION_DURATION), message=AuctionErrorMessage.END_DATE_TOO_SOON)
        sp.verify(create_auction_request.end_timestamp  <= sp.now.add_hours(MAXIMAL_AUCTION_DURATION), message=AuctionErrorMessage.END_DATE_TOO_LATE)
        sp.verify(create_auction_request.bid_amount >= MINIMAL_BID, message=AuctionErrorMessage.BID_AMOUNT_TOO_LOW)
        sp.verify(~self.data.auctions.contains(create_auction_request.auction_id), message=AuctionErrorMessage.ID_ALREADY_IN_USE)

        #set the actual auction in the auctions
        self.data.auctions[create_auction_request.auction_id] = sp.record(
        token_id=create_auction_request.token_id,
        end_timestamp=create_auction_request.end_timestamp,
        uploader=create_auction_request.uploader, 
        bid_amount=create_auction_request.bid_amount,
        voter_amount=create_auction_request.voter_amount,
        bidder=create_auction_request.uploader)
        
        #and increase the auction_id counter
        self.data.all_auctions = create_auction_request.auction_id + 1

    @sp.entry_point
    def bid(self, auction_id):
        """ 
        Entry-Point for bidding on an auction (will be called by the users)
        """
        sp.set_type_expr(auction_id, sp.TNat)
        sp.verify(self.data.auctions.contains(auction_id), message = AuctionErrorMessage.AUCTION_DOES_NOT_EXIST)
        auction = self.data.auctions[auction_id] #find the auction the user wants to bid on

        sp.verify(sp.sender != auction.uploader, message = AuctionErrorMessage.UPLOADER_CANNOT_BID)
        sp.verify(sp.amount >= auction.bid_amount + BID_STEP_THRESHOLD, message=AuctionErrorMessage.BID_AMOUNT_TOO_LOW)
        sp.verify(sp.now < auction.end_timestamp, message = AuctionErrorMessage.AUCTION_IS_OVER)

        #do not send the initial amount to the uploader because we just use this as a minimal amount for the auction
        sp.if auction.bidder != auction.uploader: 
            sp.send(auction.bidder, auction.bid_amount)
            # otherwise we transfer the previous bid_amount to the previous highest bidder

        auction.bidder = sp.sender
        auction.bid_amount = sp.amount
        
        #This will extend an auction-timeframe if an auction is bid on in the last 5 minutes. Which is common practice in tezos auctions
        
        sp.if auction.end_timestamp-sp.now < AUCTION_EXTENSION_THRESHOLD:
            auction.end_timestamp = sp.now.add_seconds(AUCTION_EXTENSION_THRESHOLD)

        self.data.auctions[auction_id] = auction

    @sp.entry_point
    def end_auction(self, auction_id):
        """ 
        Entry-Point for ending an auction. Can actually only be done by the admin because we this calls the money_pool
        """
        sp.set_type_expr(auction_id, sp.TNat)
        sp.verify(self.data.auctions.contains(auction_id), message = AuctionErrorMessage.AUCTION_DOES_NOT_EXIST)
        auction = self.data.auctions[auction_id]

        sp.verify(sp.now > auction.end_timestamp, message=AuctionErrorMessage.AUCTION_IS_ONGOING)

        # somebody bid who isn't the uploader => we actually got value
        sp.if auction.bidder != auction.uploader:
            # calculation of the shares
            bid_amount = sp.local("bid_amount", sp.utils.mutez_to_nat(auction.bid_amount))
            percentage = sp.local("percentage", bid_amount.value // sp.nat(100))
            percentage_remainder = sp.local("percentage_remainder", bid_amount.value % sp.nat(100))
            uploader_reward = sp.local("uploader_reward", percentage.value * self.data.uploader_share)
            voter_reward = sp.local("voter_reward", (percentage.value * self.data.voter_share) // auction.voter_amount)
            remainder2 = sp.local("remainder2", (percentage.value * self.data.voter_share) % auction.voter_amount)
            voter_transaction = sp.local("voter_transaction", voter_reward.value * auction.voter_amount)
            blckbook_reward = sp.local("blckbook_reward", self.data.blckbook_share * percentage.value + percentage_remainder.value + remainder2.value)

            money_pool_contract = sp.contract(AuctionReward.get_type(), self.data.money_pool, entry_point = "enter_auction").open_some()

            sp.send(auction.uploader, sp.utils.nat_to_mutez(uploader_reward.value))
            sp.send(self.data.blckbook_collector, sp.utils.nat_to_mutez(blckbook_reward.value))
            sp.transfer(
                sp.record(auction_id=auction.token_id, reward=sp.utils.nat_to_mutez(voter_reward.value)), 
                sp.utils.nat_to_mutez(voter_transaction.value),
                money_pool_contract,
            )

        token_contract = sp.contract(BatchTransfer.get_type(), self.data.token_address, entry_point = "transfer").open_some()

        # we always transfer to the highest-bidder which could be the uploader (if no-one bid on the auction)
        sp.transfer([BatchTransfer.item(sp.self_address, [sp.record(to_=auction.bidder, token_id=auction.token_id, amount=sp.nat(1))])],
        sp.mutez(0), token_contract)

        del self.data.auctions[auction_id] #this will delete the auction-entry (so we reduce the storage-diff)- otherwise make it so an auction can not be ended twice

class AddVote():
    """ 
    The data-type class for adding votes on a specific artwork
    """
    def get_type():
        return sp.TRecord(
            voter_addresses=sp.TList(sp.TAddress), 
            auction_id=sp.TNat,
        ).layout(("voter_addresses","auction_id"))

class MoneyPoolErrorMessage:
    PREFIX = "POOL_"
    NOT_ADMIN = "{}NOT_ADMIN".format(PREFIX)
    AUCTION_ALREADY_RESOLVED = "{}AUCTION_ALREADY_RESOLVED".format(PREFIX)
    NOT_A_VOTER = "{}NOT_A_VOTER_OR_ALREADY_WITHDRAWN".format(PREFIX)
    ALL_VOTES_ALREADY_PAYED_OUT = "{}ALL_VOTES_ALREADY_PAYED_OUT".format(PREFIX)

class AuctionReward():
    """ 
    The data-type class for adding a reward in mutez 
    """
    def get_type():
        return sp.TRecord(
            auction_id=sp.TNat,
            reward=sp.TMutez)

class MoneyPool(sp.Contract):
    def __init__(self, administrator):
        sp.set_type_expr(administrator, sp.TAddress)
        self.init_type(sp.TRecord(
                administrator = sp.TAddress,
                auctions = sp.TBigMap(sp.TNat, sp.TMutez),
                vote_map = sp.TBigMap(sp.TAddress, sp.TList(sp.TNat)),
        ).layout(("administrator", ("auctions", "vote_map"))))

        self.init(
            administrator = administrator,
            auctions=sp.big_map(tkey=sp.TNat, tvalue = sp.TMutez),        
            vote_map = sp.big_map(tkey=sp.TAddress, tvalue=sp.TList(sp.TNat)),
        )   
     
    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, MoneyPoolErrorMessage.NOT_ADMIN)
        self.data.administrator = params

    @sp.entry_point
    def enter_auction(self, params):
        # maybe change this so a user can resolve the auction to check for sender = AuctionHouse
        sp.verify(sp.source == self.data.administrator, MoneyPoolErrorMessage.NOT_ADMIN)
        sp.set_type_expr(params, AuctionReward.get_type())
        sp.verify(~self.data.auctions.contains(params.auction_id), MoneyPoolErrorMessage.AUCTION_ALREADY_RESOLVED)
        self.data.auctions[params.auction_id] = params.reward

    @sp.entry_point
    def add_votes(self, votes):
        sp.verify(sp.sender == self.data.administrator) # only admin can create auction (nft needs to be minted for auction-contract)
        sp.set_type_expr(votes, AddVote.get_type())
        sp.for vote in votes.voter_addresses:
            self.data.vote_map[vote] = sp.sp.cons(votes.auction_id, self.data.vote_map.get(vote, default_value = []))

    @sp.entry_point
    def withdraw(self):
        sp.verify(self.data.vote_map.contains(sp.sender), MoneyPoolErrorMessage.NOT_A_VOTER)
        sum = sp.local("sum", sp.mutez(0))
        not_resolved_yet = sp.local('not_resolved_yet', sp.list([], t = sp.TNat))
        sp.for auction in self.data.vote_map[sp.sender]:
            sp.if self.data.auctions.contains(auction):
                sum.value = sum.value + self.data.auctions[auction]
            sp.else:
                not_resolved_yet.value.push(auction)
        
        self.data.vote_map[sp.sender] = not_resolved_yet.value

        sp.if (sum.value > sp.mutez(0)):
            sp.send(sp.sender, sum.value)
        sp.else:
            sp.failwith(MoneyPoolErrorMessage.ALL_VOTES_ALREADY_PAYED_OUT)

class BatchTransfer():
    def get_transfer_type():
        tx_type = sp.TRecord(to_ = sp.TAddress,
                             token_id = sp.TNat,
                             amount = sp.TNat)
        tx_type = tx_type.layout(
            ("to_", ("token_id", "amount"))
        )
        transfer_type = sp.TRecord(from_ = sp.TAddress,
                                   txs = sp.TList(tx_type)).layout(
                                       ("from_", "txs"))
        return transfer_type

    def get_type():
        return sp.TList(BatchTransfer.get_transfer_type())

    def item(from_, txs):
        return sp.set_type_expr(sp.record(from_ = from_, txs = txs), BatchTransfer.get_transfer_type())

class TestHelper():
    def make_metadata(symbol, name, decimals):
        "Helper function to build metadata JSON bytes values."
        return (sp.map(l = {
            # Remember that michelson wants map already in ordered
            "decimals" : sp.utils.bytes_of_string("%d" % decimals),
            "name" : sp.utils.bytes_of_string(name),
            "symbol" : sp.utils.bytes_of_string(symbol)
        }))

    def ledger_key_make(user, token):
        user = sp.set_type_expr(user, sp.TAddress)
        token = sp.set_type_expr(token, sp.TNat)
        result = sp.pair(user, token)
        return result

@sp.add_test(name = "my_test")
def test():
    scenario = sp.test_scenario()
    scenario.h1("FA2 Contract Name:")
    scenario.table_of_contents()
    # sp.test_account generates ED25519 key-pairs deterministically:
    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob   = sp.test_account("Robert")
    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob])
    c1 = Contract(admin.address)
    scenario += c1
    scenario.h2("Initial Minting")
    scenario.p("The administrator mint 1 token-0's to Alice.")
    tok0_md = TestHelper.make_metadata(
        name = "The Token Zero",
        decimals = 2,
        symbol= "TK0" )
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok0_md,
                        token_id = 0).run(sender = admin)
    scenario.h2("Transfers Alice -> Bob")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 0)
                                ])
        ]).run(sender = alice)
    scenario.verify(
        c1.data.ledger[TestHelper.ledger_key_make(alice.address, 0)].balance == 0)
    scenario.verify(
        c1.data.ledger[TestHelper.ledger_key_make(bob.address, 0)].balance == 1)
    scenario.verify(
        c1.data.ledger[TestHelper.ledger_key_make(alice.address, 0)].balance == 0
    )
    scenario.verify(
        c1.data.ledger[TestHelper.ledger_key_make(bob.address, 0)].balance == 1
    )

    scenario.h2("Only admin should be able to mint")
    tok42_md = TestHelper.make_metadata(
        name = "The Token 42",
        decimals = 0,
        symbol= "TK42")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok42_md,
                        token_id = 42).run(valid=False, sender = alice)


    scenario.h2("More Token Types")
    tok1_md = TestHelper.make_metadata(
        name = "The Second Token",
        decimals = 0,
        symbol= "TK1" )
    c1.mint(address = bob.address,
                        amount = 1,
                        metadata = tok1_md,
                        token_id = 1).run(sender = admin)
    tok2_md = TestHelper.make_metadata(
        name = "The Token Number Three",
        decimals = 0,
        symbol= "TK2" )
    c1.mint(address = bob.address,
                        amount = 1,
                        metadata = tok2_md,
                        token_id = 2).run(sender = admin)
    scenario.h3("Multi-token Transfer Bob -> Alice")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 1)]),
            # We voluntarily test a different sub-batch:
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 2)])
        ]).run(sender = bob)
    scenario.h2("Other Basic Permission Tests")
    scenario.h3("Bob cannot transfer Alice's tokens.")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 0),
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 0)])
        ]).run(sender = bob, valid = False)
    scenario.h3("Admin can transfer anything.")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 0)]),
        ]).run(sender = admin)
   
    scenario.h1("Burning")
    tok_burn = TestHelper.make_metadata(
        name = "The token to burn",
        decimals = 0,
        symbol= "TK42")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok_burn,
                        token_id = 3).run(valid=True, sender = admin)

    scenario.h2("User can not burn a token")
    c1.burn(address = alice.address, token_id = 3).run(valid=False, sender = alice);

    scenario.h2("Admin can not burn token if it is not in the right ledger")
    c1.burn(address = bob.address, token_id = 3).run(valid=False, sender = admin);

    scenario.h2("Admin can burn a token")
    c1.burn(address = alice.address, token_id = 3).run(valid=True, sender = admin);

    scenario.h2("Can not transfer a token after the burn")
    c1.transfer(
    [
        BatchTransfer.item(from_ = alice.address,
                            txs = [
                                sp.record(to_ = bob.address,
                                          amount = 1,
                                          token_id = 3)]),
    ]).run(valid=False, sender = alice)

    scenario.h2("Admin can not burn a token twice")
    c1.burn(address = alice.address, token_id = 3).run(valid=False, sender = admin);

    scenario.table_of_contents()

@sp.add_test(name = "Auction House")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Auction House")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    money_pool = MoneyPool(admin.address)
    scenario += money_pool

    blckbook_collector = sp.test_account("blckbookcollector")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan, blckbook_collector])

    fa2 = Contract(admin.address)
    scenario += fa2

    auction_house = AuctionHouse(administrator=admin.address, 
        money_pool = money_pool.address, 
        blckbook_collector = blckbook_collector.address,
        token_address = fa2.address)
    scenario += auction_house

    scenario.h2("Check that we dont leave money in the auction-contract")

    tok0_md = TestHelper.make_metadata(
        name = "The Token Zero",
        decimals = 0,
        symbol= "TK0" )
    fa2.mint(address = auction_house.address,
                        amount = 1,
                        metadata = tok0_md,
                        token_id = 0).run(sender = admin)

    scenario += auction_house.create_auction(
        auction_id=sp.nat(0),
        token_id=sp.nat(0), 
        end_timestamp=sp.timestamp(0).add_days(7),  
        voter_amount=sp.nat(12365),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    scenario.p("Bob bids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(2000000), now=sp.timestamp(0).add_minutes(1))

    scenario.p("Dan bids")
    scenario += auction_house.bid(0).run(sender=dan,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(2))

    scenario.p("Bob rebids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(3))

    scenario.p("Admin ends auction")

    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    #can not end an auction twice
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7), valid=False)

    scenario.verify(auction_house.balance  == sp.mutez(0))

    fa2.transfer(
    [
        BatchTransfer.item(from_ = bob.address,
                            txs = [
                                sp.record(to_ = alice.address,
                                          amount = 1,
                                          token_id = 0)
                            ])
    ]).run(sender = bob) #just to make sure the token is now in bob's posession


    scenario.h2("Set shares to weird amounts and let the entry-point fail")
    auction_house.set_shares(blckbook_share = sp.nat(100), uploader_share = sp.nat(100), voter_share = sp.nat(100)).run(sender=admin, valid=False)
    auction_house.set_shares(blckbook_share = sp.nat(90), uploader_share = sp.nat(10), voter_share = sp.nat(10)).run(sender=admin, valid=False)
    auction_house.set_shares(blckbook_share = sp.nat(42), uploader_share = sp.nat(42), voter_share = sp.nat(10)).run(sender=admin, valid=False)


    scenario.h2("Set shares to a correct amount")
    auction_house.set_shares(blckbook_share = sp.nat(10), uploader_share = sp.nat(80), voter_share = sp.nat(10)).run(sender=admin)

    scenario.h2("Only admin can set shares")
    auction_house.set_shares(blckbook_share = sp.nat(10), uploader_share = sp.nat(80), voter_share = sp.nat(10)).run(sender=bob, valid=False)

    scenario.h2("Try to bid on auctions, that do not exist")
    scenario += auction_house.bid(420).run(sender=bob,amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(3), valid=False)

@sp.add_test(name = "Money Pool")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Money Pool")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan])

    money_pool = MoneyPool(admin.address)
    scenario += money_pool
    scenario.h2("Add votes")
    
    money_pool.add_votes(sp.record(
            voter_addresses=[alice.address, bob.address], 
            auction_id=sp.nat(0),
        )).run(sender=admin)

    scenario.h3("Alice should not be able to withdraw before the reward gets set")
    money_pool.withdraw().run(sender=alice, valid=False)

    money_pool.enter_auction(auction_id=sp.nat(0), reward=sp.mutez(200)).run(sender=admin, amount=sp.mutez(400))

    money_pool.withdraw().run(sender=alice)
    scenario.verify(money_pool.balance  == sp.mutez(200))
    money_pool.withdraw().run(sender=bob)
    scenario.verify(money_pool.balance  == sp.mutez(0))

    money_pool.withdraw().run(sender=alice, valid=False)

    scenario.h2("Add multiple votes add withdraw between rounds")

    money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address], 
        auction_id=sp.nat(1),
    )).run(sender=admin)

    money_pool.add_votes(sp.record( 
        voter_addresses=[alice.address, bob.address], 
        auction_id=sp.nat(2),
    )).run(sender=admin)

    money_pool.enter_auction(auction_id=sp.nat(1), reward=sp.mutez(300)).run(sender=admin, amount=sp.mutez(600))
    money_pool.withdraw().run(sender=alice)
    scenario.verify(money_pool.balance  == sp.mutez(300))

    money_pool.enter_auction(auction_id=sp.nat(2), reward=sp.mutez(400)).run(sender=admin, amount=sp.mutez(800))
    scenario.verify(money_pool.balance  == sp.mutez(1100))
    money_pool.withdraw().run(sender=alice)
    scenario.verify(money_pool.balance  == sp.mutez(700))
    money_pool.withdraw().run(sender=bob)
    scenario.verify(money_pool.balance  == sp.mutez(0))

@sp.add_test(name = "All together")
def test():
    scenario = sp.test_scenario()
    scenario.h1("All together")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    money_pool = MoneyPool(admin.address)
    scenario += money_pool

    blckbook_collector = sp.test_account("blckbookcollector")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan, blckbook_collector])

    fa2 = Contract(admin.address)
    scenario += fa2

    auction_house = AuctionHouse(administrator=admin.address, 
        money_pool = money_pool.address, 
        blckbook_collector = blckbook_collector.address,
        token_address = fa2.address)
    scenario += auction_house

    scenario.h2("3 Votes for an NFT that doesn't get bid on")

    tok0_md = TestHelper.make_metadata(
        name = "The Token Zero",
        decimals = 0,
        symbol= "TK0" )
    fa2.mint(address = auction_house.address,
                        amount = 1,
                        metadata = tok0_md,
                        token_id = 0).run(sender = admin)

    scenario += auction_house.create_auction(
        auction_id=sp.nat(0),
        token_id=sp.nat(0), 
        end_timestamp=sp.timestamp(0).add_days(7),  
        voter_amount=sp.nat(3),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address, dan.address], 
        auction_id=sp.nat(0),
    )).run(sender=admin)

    scenario.h3("end the auction")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    scenario.h3("transfer the item back to the auction_house so it can have another auction created")
    fa2.transfer(
    [
        BatchTransfer.item(from_ = alice.address,
                            txs = [
                                sp.record(to_ = auction_house.address,
                                          amount = 1,
                                          token_id = 0)
                            ])
    ]).run(sender = alice)

    money_pool.withdraw().run(sender=alice, valid=False)

    scenario.h3("create another auction for the same token (wont happen on chain but is okay for a test)")
    scenario += auction_house.create_auction(
        auction_id=sp.nat(1),
        token_id=sp.nat(0), 
        end_timestamp=sp.timestamp(0).add_days(14),  #has to be like 2 weeks later
        voter_amount=sp.nat(3),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address, dan.address], 
        auction_id=sp.nat(1),
    )).run(sender=admin)

    scenario.h3("dan bids")
    scenario += auction_house.bid(1).run(sender=dan,amount=sp.mutez(2000000), now=sp.timestamp(0).add_minutes(1))

    scenario.h2("alice tries to bid on weird things")
    scenario.h3("she can't bid cause she is the owner")
    scenario += auction_house.bid(1).run(sender=alice,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(2), valid=False)
    scenario.h3("she can't bid cause the auction is not existent anymore")
    scenario += auction_house.bid(0).run(sender=alice,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(2), valid=False)
    scenario.h3("she can't bid cause the auction never existed")
    scenario += auction_house.bid(3).run(sender=alice,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(2), valid=False)

    scenario.h2("back to the normal run")

    scenario.h3("dan bids")
    scenario += auction_house.bid(1).run(sender=dan,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(2))

    scenario.h3("bob tries to bid a very small increase")
    scenario += auction_house.bid(1).run(sender=bob,amount=sp.mutez(3000001), now=sp.timestamp(0).add_minutes(2), valid=False)

    scenario.h3("end the auction")
    scenario += auction_house.end_auction(1).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(14))
    
    scenario.verify(auction_house.balance  == sp.mutez(0))

    scenario.verify(money_pool.balance == sp.mutez(450000))
    money_pool.withdraw().run(sender=alice)
    scenario.verify(money_pool.balance == sp.mutez(300000))
    money_pool.withdraw().run(sender=bob)
    scenario.verify(money_pool.balance == sp.mutez(150000))
    money_pool.withdraw().run(sender=dan)
    scenario.verify(money_pool.balance == sp.mutez(0))