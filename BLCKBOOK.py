import smartpy as sp

class BatchTransfer():
    """
    Class for the transfer endpoint of the FA2-Token contract
    """
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

class Operator:
    def get_type():
        t = sp.TRecord(
            owner = sp.TAddress,
            operator = sp.TAddress,
            token_id = sp.TNat).layout(("owner", ("operator", "token_id")))
        return t
    def make(owner, operator, token_id):
        r = sp.record(owner = owner,
                      operator = operator,
                      token_id = token_id)
        return sp.set_type_expr(r, Operator.get_type())

class TokensContract(sp.Contract):
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
                operators = sp.TBigMap(Operator.get_type(), sp.TUnit),
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
            operators = sp.big_map(tkey = Operator.get_type(), tvalue = sp.TUnit),
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
        sp.verify(~ (params.token_id < self.data.all_tokens), 'NFT-asset: cannot mint the same token twice')
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

class AuctionCreationParams():
    """ 
    The data-type class for creating a new auction
    """
    def get_type():
        return sp.TRecord(
        auction_and_token_id=sp.TNat,
        end_timestamp=sp.TTimestamp,  
        voter_amount=sp.TNat,
        uploader=sp.TAddress,
        bid_amount=sp.TMutez,
        ).layout(("auction_and_token",("end_timestamp",("voter_amount",("uploader","bid_amount")))))

class Auction():
    """ 
    The data-type class for a single auction contained in the auction-house-contract
    """
    def get_type():
        return sp.TRecord(
            end_timestamp=sp.TTimestamp,
            voter_amount=sp.TNat,
            uploader=sp.TAddress,
            bid_amount=sp.TMutez, #holds the current bid (at the start the minimal bid)
            bidder=sp.TAddress,
        ).layout(("end_timestamp",("voter_amount",("uploader",("bid_amount","bidder")))))


class AuctionHouseContract(sp.Contract):
    """ 
    The smart contract for the actual Auction-House
    """
    def __init__(self, administrator, blckbook_collector, voter_money_pool, tokens_contract_address):
        sp.set_type_expr(administrator, sp.TAddress)
        sp.set_type_expr(blckbook_collector, sp.TAddress)
        sp.set_type_expr(voter_money_pool, sp.TAddress)
        sp.set_type_expr(tokens_contract_address, sp.TAddress)
        self.init_type(sp.TRecord(
                administrator = sp.TAddress,
                blckbook_collector = sp.TAddress,
                voter_money_pool = sp.TAddress,
                tokens_contract_address = sp.TAddress,
                blckbook_share=sp.TNat,
                uploader_share=sp.TNat,
                voter_share=sp.TNat,
                auctions = sp.TBigMap(sp.TNat, Auction.get_type()),
                all_auctions = sp.TNat,
        ).layout(("administrator", ("blckbook_collector", ("voter_money_pool", ("tokens_contract_address", ("blckbook_share", ("uploader_share", ("voter_share", ("all_auctions", "auctions"))))))))))

        self.init(blckbook_share = sp.nat(25),
                    voter_share = sp.nat(15),
                    uploader_share = sp.nat(60),
                    auctions=sp.big_map(tkey=sp.TNat, tvalue = Auction.get_type()),
                    blckbook_collector = blckbook_collector,
                    administrator = administrator,
                    tokens_contract_address = tokens_contract_address,
                    voter_money_pool = voter_money_pool,
                    all_auctions= sp.nat(0))
     

    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.administrator = params

    @sp.entry_point
    def set_tokens_contract_address(self, params):
        """ 
        Entry-Point for setting the FA2-Contract Address
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.tokens_contract_address = params

    @sp.entry_point
    def set_blckbook_collector(self, params):
        """ 
        Entry-Point for setting the address of the blckbook_collector which will get the blckbook share of the auction-prices
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.blckbook_collector = params
    
    @sp.entry_point
    def set_voter_money_pool_address(self, params):
        """ 
        Entry-Point for setting the address of the voter_money_pool which will get the shares for the voters and will get called with the info how much every voter gets
        """
        sp.verify(sp.sender == self.data.administrator, AuctionErrorMessage.NOT_ADMIN)
        self.data.voter_money_pool = params

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
        sp.set_type_expr(create_auction_request, AuctionCreationParams.get_type())

        sp.verify(~(create_auction_request.auction_and_token_id < self.data.all_auctions), message=AuctionErrorMessage.CAN_NOT_CREATE_AN_AUCTION_TWICE)
        sp.verify(self.data.all_auctions == create_auction_request.auction_and_token_id, message=AuctionErrorMessage.AUCTION_ID_SHOULD_BE_CONSECUTIVE)

        sp.verify(create_auction_request.end_timestamp  >= sp.now.add_hours(MINIMAL_AUCTION_DURATION), message=AuctionErrorMessage.END_DATE_TOO_SOON)
        sp.verify(create_auction_request.end_timestamp  <= sp.now.add_hours(MAXIMAL_AUCTION_DURATION), message=AuctionErrorMessage.END_DATE_TOO_LATE)
        sp.verify(create_auction_request.bid_amount >= MINIMAL_BID, message=AuctionErrorMessage.BID_AMOUNT_TOO_LOW)
        sp.verify(~self.data.auctions.contains(create_auction_request.auction_and_token_id), message=AuctionErrorMessage.ID_ALREADY_IN_USE)

        #set the actual auction in the auctions
        self.data.auctions[create_auction_request.auction_and_token_id] = sp.record(
        end_timestamp=create_auction_request.end_timestamp,
        uploader=create_auction_request.uploader, 
        bid_amount=create_auction_request.bid_amount,
        voter_amount=create_auction_request.voter_amount,
        bidder=create_auction_request.uploader)
        
        #and increase the auction_and_token_id counter
        self.data.all_auctions = create_auction_request.auction_and_token_id + 1

    @sp.entry_point
    def bid(self, auction_and_token_id):
        """ 
        Entry-Point for bidding on an auction (will be called by the users)
        """
        sp.set_type_expr(auction_and_token_id, sp.TNat)
        sp.verify(self.data.auctions.contains(auction_and_token_id), message = AuctionErrorMessage.AUCTION_DOES_NOT_EXIST)
        auction = self.data.auctions[auction_and_token_id] #find the auction the user wants to bid on

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

        self.data.auctions[auction_and_token_id] = auction

    @sp.entry_point
    def end_auction(self, auction_and_token_id):
        """ 
        Entry-Point for ending an auction. Can actually only be done by the admin because we this calls the voter_money_pool
        """
        sp.set_type_expr(auction_and_token_id, sp.TNat)
        sp.verify(self.data.auctions.contains(auction_and_token_id), message = AuctionErrorMessage.AUCTION_DOES_NOT_EXIST)
        auction = self.data.auctions[auction_and_token_id]

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

            voter_money_pool_contract = sp.contract(SetAuctionRewardParams.get_type(), self.data.voter_money_pool, entry_point = "set_auction_rewards").open_some()

            sp.send(auction.uploader, sp.utils.nat_to_mutez(uploader_reward.value))
            sp.send(self.data.blckbook_collector, sp.utils.nat_to_mutez(blckbook_reward.value))
            sp.transfer(
                sp.record(auction_and_token_id = auction_and_token_id, reward=sp.utils.nat_to_mutez(voter_reward.value)), 
                sp.utils.nat_to_mutez(voter_transaction.value),
                voter_money_pool_contract,
            )

        token_contract = sp.contract(BatchTransfer.get_type(), self.data.tokens_contract_address, entry_point = "transfer").open_some()

        # we always transfer to the highest-bidder which could be the uploader (if no-one bid on the auction)
        sp.transfer([BatchTransfer.item(sp.self_address, [sp.record(to_=auction.bidder, token_id=auction_and_token_id, amount=sp.nat(1))])],
        sp.mutez(0), token_contract)

        del self.data.auctions[auction_and_token_id] #this will delete the auction-entry (so we reduce the storage-diff)- otherwise make it so an auction can not be ended twice

class AddVotesParams():
    """ 
    The data-type class for adding votes to a single auction (and its corresponding token)
    """
    def get_type():
        return sp.TRecord(
            voter_addresses=sp.TList(sp.TAddress), 
            auction_and_token_id=sp.TNat,
        ).layout(("voter_addresses","auction_and_token_id"))

class VoterMoneyPoolErrorMessage:
    PREFIX = "VOTER_MONEY_POOL_"
    NOT_ADMIN = "{}NOT_ADMIN".format(PREFIX)
    AUCTION_ALREADY_RESOLVED = "{}AUCTION_ALREADY_RESOLVED".format(PREFIX)
    NOT_A_VOTER = "{}NOT_A_VOTER".format(PREFIX)
    ALL_VOTES_ALREADY_PAYED_OUT = "{}ALL_VOTES_ALREADY_PAYED_OUT".format(PREFIX)

class SetAuctionRewardParams():
    """ 
    The data-type class for setting the voter_rewards for a specific auction (and it's corresponding token)
    """
    def get_type():
        return sp.TRecord(
            auction_and_token_id=sp.TNat,
            reward=sp.TMutez)

class VoterMoneyPoolContract(sp.Contract):
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
        sp.verify(sp.sender == self.data.administrator, VoterMoneyPoolErrorMessage.NOT_ADMIN)
        self.data.administrator = params

    @sp.entry_point
    def set_auction_rewards(self, params):
        # maybe change this so a user can resolve the auction to check for sender = AuctionHouseContract
        sp.verify(sp.source == self.data.administrator, VoterMoneyPoolErrorMessage.NOT_ADMIN)
        sp.set_type_expr(params, SetAuctionRewardParams.get_type())
        sp.verify(~self.data.auctions.contains(params.auction_and_token_id), VoterMoneyPoolErrorMessage.AUCTION_ALREADY_RESOLVED)
        self.data.auctions[params.auction_and_token_id] = params.reward

    @sp.entry_point
    def add_votes(self, votes):
        sp.verify(sp.sender == self.data.administrator) # only admin can create auction (nft needs to be minted for auction-contract)
        sp.set_type_expr(votes, AddVotesParams.get_type())
        sp.for vote in votes.voter_addresses:
            self.data.vote_map[vote] = sp.sp.cons(votes.auction_and_token_id, self.data.vote_map.get(vote, default_value = []))

    @sp.entry_point
    def withdraw(self):
        sp.verify(self.data.vote_map.contains(sp.sender), VoterMoneyPoolErrorMessage.NOT_A_VOTER)
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
            sp.failwith(VoterMoneyPoolErrorMessage.ALL_VOTES_ALREADY_PAYED_OUT)

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

@sp.add_test(name = "FA2-Contract Test")
def test():
    scenario = sp.test_scenario()
    scenario.h1("BLCKBOOK FA2 Contract")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob   = sp.test_account("Bob")
    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob])
    c1 = TokensContract(admin.address)
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

    scenario.h2("Only admin should be able to mint")
    tok42_md = TestHelper.make_metadata(
        name = "The Token 42",
        decimals = 0,
        symbol= "TK42")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok42_md,
                        token_id = 1).run(valid=False, sender = alice)


    scenario.h2("Admin can not create a token with an index that is non-consecutive")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok42_md,
                        token_id = 42).run(valid=False, sender = admin)

    scenario.h2("Admin can not create a token that already exists")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok42_md,
                        token_id = 0).run(valid=False, sender = admin)

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
        symbol= "BLCKBOOK" )
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

    scenario.h2("Admin can not burn a token that never existed")
    c1.burn(address = alice.address, token_id = 10).run(valid=False, sender = admin);

    scenario.table_of_contents()

@sp.add_test(name = "FA2-Contract Operator Test")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Operators FA2 Contract")
    scenario.table_of_contents()
    
    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob   = sp.test_account("Bob")
    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob])
    c1 = TokensContract(admin.address)

    scenario += c1
    
    scenario.p("Calling 0 updates should work:")
    c1.update_operators([]).run()
    scenario.h3("Operator Accounts")
    op0 = sp.test_account("Operator0")
    op1 = sp.test_account("Operator1")
    op2 = sp.test_account("Operator2")
    scenario.show([op0, op1, op2])

    tok1_md = TestHelper.make_metadata(
        name = "Random Token",
        decimals = 0,
        symbol= "TK1")
    c1.mint(address = alice.address,
                        amount = 1,
                        metadata = tok1_md,
                        token_id = 0).run(sender = admin)

    c1.mint(address = alice.address,
                    amount = 1,
                    metadata = tok1_md,
                    token_id = 1).run(sender = admin)

    scenario.p("Admin can change Alice's operator.")

    scenario.p("Operator2 cannot transfer Alice's tokens")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op2, valid = False)

    c1.update_operators([
        sp.variant("add_operator", Operator.make(
            owner = alice.address,
            operator = op1.address,
            token_id = 0)),
        sp.variant("add_operator", Operator.make(
            owner = alice.address,
            operator = op1.address,
            token_id = 1))
    ]).run(sender = admin)
    scenario.p("Operator1 can now transfer Alice's tokens 0 and 2")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                    txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 0),
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op1)
    scenario.p("Operator1 cannot transfer Bob's tokens")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = op1.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op1, valid = False)
    scenario.p("Alice can remove their operator")
    c1.update_operators([
        sp.variant("remove_operator", Operator.make(
            owner = alice.address,
            operator = op1.address,
            token_id = 0)),
        sp.variant("remove_operator", Operator.make(
            owner = alice.address,
            operator = op1.address,
            token_id = 1))
    ]).run(sender = alice)
    scenario.p("Operator1 cannot transfer Alice's tokens any more")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = op1.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op1, valid = False)
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = op1.address,
                                              amount = 1,
                                              token_id = 0)])
        ]).run(sender = op1, valid = False)
    scenario.p("Bob can add Operator0.")
    c1.update_operators([
        sp.variant("add_operator", Operator.make(
            owner = bob.address,
            operator = op0.address,
            token_id = 0)),
        sp.variant("add_operator", Operator.make(
            owner = bob.address,
            operator = op0.address,
            token_id = 1))
    ]).run(sender = bob)
    scenario.p("Operator0 can transfer Bob's tokens '0' and '1'")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 0)]),
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op0)
    scenario.p("Bob cannot add Operator0 for Alice's tokens.")
    c1.update_operators([
        sp.variant("add_operator", Operator.make(
            owner = alice.address,
            operator = op0.address,
            token_id = 0
        ))
    ]).run(sender = bob, valid = False)
    scenario.p("Alice can also add Operator0 for their tokens 0.")
    c1.update_operators([
        sp.variant("add_operator", Operator.make(
            owner = alice.address,
            operator = op0.address,
            token_id = 0
        )),
        sp.variant("add_operator", Operator.make(
            owner = alice.address,
            operator = op0.address,
            token_id = 1
        ))
    ]).run(sender = alice, valid = True)

    scenario.p("Operator0 can now transfer Alice's 0-tokens.")
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 0)]),
        ]).run(sender = op0)

    scenario.p("Operator0 can now transfer Bob's and Alice's tokens.")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 0)]),
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op0)
    scenario.p("Bob adds Operator2 as second operator for 1-tokens.")
    c1.update_operators([
        sp.variant("add_operator", Operator.make(
            owner = bob.address,
            operator = op2.address,
            token_id = 1
        ))
    ]).run(sender = bob, valid = True)
    scenario.p("Operator0 and Operator2 can transfer Bob's 1-tokens.")
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op0)
    c1.transfer(
        [
            BatchTransfer.item(from_ = alice.address,
                                txs = [
                                    sp.record(to_ = bob.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = alice)
    c1.transfer(
        [
            BatchTransfer.item(from_ = bob.address,
                                txs = [
                                    sp.record(to_ = alice.address,
                                              amount = 1,
                                              token_id = 1)])
        ]).run(sender = op2)
    scenario.table_of_contents()


@sp.add_test(name = "Auction House Contract Test")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Auction House")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    voter_money_pool = VoterMoneyPoolContract(admin.address)
    scenario += voter_money_pool

    blckbook_collector = sp.test_account("blckbookcollector")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan, blckbook_collector])

    fa2 = TokensContract(admin.address)
    scenario += fa2

    auction_house = AuctionHouseContract(administrator=admin.address, 
        voter_money_pool = voter_money_pool.address, 
        blckbook_collector = blckbook_collector.address,
        tokens_contract_address = fa2.address)
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
        auction_and_token_id=sp.nat(0),
        end_timestamp=sp.timestamp(0).add_days(7),  
        voter_amount=sp.nat(12365),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    scenario.h3("Bob bids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(2000000), now=sp.timestamp(0).add_minutes(1))

    scenario.h3("bob tries to bid a increase that is too small")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(2000001), now=sp.timestamp(0).add_minutes(2), valid=False)

    scenario.h3("dan bids again (with a higher sum)")
    scenario += auction_house.bid(0).run(sender=dan,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(3))

    scenario.h3("Bob rebids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(4))

    scenario.h3("Admin can not end a running auction")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(8), valid=False)

    scenario.h3("Alice can't bid cause she is the uploader")
    scenario += auction_house.bid(0).run(sender=alice,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(10), valid=False)
    
    scenario.h2("Try to bid on auctions, that is over")
    scenario += auction_house.bid(0).run(sender=dan, amount=sp.mutez(501001327), now=sp.timestamp(0).add_minutes(5).add_days(7), valid=False)

    scenario.h2("Admin ends auction")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    scenario.h2("Admin can not end an auction twice")
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

    scenario.h2("Try to bid on an auction, that does not exist")
    scenario += auction_house.bid(420).run(sender=bob, amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(3), valid=False)

    scenario.h2("Try to bid on an auction, that is already resolved")
    scenario += auction_house.bid(0).run(sender=bob, amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(3), valid=False)

@sp.add_test(name = "Voter Money Pool")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Voter Money Pool")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan])

    voter_money_pool = VoterMoneyPoolContract(admin.address)
    scenario += voter_money_pool
    scenario.h2("Add votes")

    scenario.h3("Alice should not be able to withdraw before a vote for her is set")
    voter_money_pool.withdraw().run(sender=alice, valid=False)
    
    voter_money_pool.add_votes(sp.record(
            voter_addresses=[alice.address, bob.address], 
            auction_and_token_id=sp.nat(0),
        )).run(sender=admin)

    scenario.h3("Alice should not be able to withdraw before the reward gets set")
    voter_money_pool.withdraw().run(sender=alice, valid=False)

    scenario.h3("Admin should be able to set voter rewards")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(0), reward=sp.mutez(200)).run(sender=admin, amount=sp.mutez(400))

    scenario.h3("Admin should not be able to set voter rewards for the same auction twice")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(0), reward=sp.mutez(200)).run(sender=admin, amount=sp.mutez(400), valid=False)

    scenario.h3("Now Alice can withdraw")
    voter_money_pool.withdraw().run(sender=alice)
    scenario.verify(voter_money_pool.balance  == sp.mutez(200))
    scenario.h3("Bob can also withdraw now")
    voter_money_pool.withdraw().run(sender=bob)
    scenario.verify(voter_money_pool.balance  == sp.mutez(0))

    scenario.h3("Alice can not withdraw again")
    voter_money_pool.withdraw().run(sender=alice, valid=False)

    scenario.h2("Add multiple votes and withdraw between setting the voter rewards")

    voter_money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address], 
        auction_and_token_id=sp.nat(1),
    )).run(sender=admin)

    voter_money_pool.add_votes(sp.record( 
        voter_addresses=[alice.address, bob.address], 
        auction_and_token_id=sp.nat(2),
    )).run(sender=admin)

    scenario.h3("Set voter rewards for auction 1")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(1), reward=sp.mutez(300)).run(sender=admin, amount=sp.mutez(600))
    voter_money_pool.withdraw().run(sender=alice)
    scenario.h3("Alice can withdraw for auction 1")
    scenario.verify(voter_money_pool.balance  == sp.mutez(300))

    scenario.h3("Set voter rewards for auction 2")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(2), reward=sp.mutez(400)).run(sender=admin, amount=sp.mutez(800))
    scenario.verify(voter_money_pool.balance  == sp.mutez(1100))
    scenario.h3("Now alice can withdraw for auction 2")
    voter_money_pool.withdraw().run(sender=alice)
    scenario.verify(voter_money_pool.balance == sp.mutez(700))
    scenario.h3("Now bob withdraws and gets the amount for auction 1 and 2")
    voter_money_pool.withdraw().run(sender=bob)
    scenario.verify(voter_money_pool.balance  == sp.mutez(0))

    scenario.h3("Alice should not be able to withdraw again when no auction is pending")
    voter_money_pool.withdraw().run(sender=alice, valid=False)

@sp.add_test(name = "Integration Tests")
def test():
    scenario = sp.test_scenario()
    scenario.h1("Integration Tests")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Robert")
    dan = sp.test_account("Dan")

    voter_money_pool = VoterMoneyPoolContract(admin.address)
    scenario += voter_money_pool

    blckbook_collector = sp.test_account("blckbookcollector")

    # Let's display the accounts:
    scenario.h2("Accounts")
    scenario.show([admin, alice, bob, dan, blckbook_collector])

    fa2 = TokensContract(admin.address)
    scenario += fa2

    auction_house = AuctionHouseContract(administrator=admin.address, 
        voter_money_pool = voter_money_pool.address, 
        blckbook_collector = blckbook_collector.address,
        tokens_contract_address = fa2.address)
    scenario += auction_house

    scenario.h2("3 Votes for an NFT that doesn't get bid on")

    tok0_md = TestHelper.make_metadata(
        name = "The Token Zero",
        decimals = 0,
        symbol= "TK0" )
    scenario.h3("Mint the tokens")
    fa2.mint(address = auction_house.address,
                        amount = 1,
                        metadata = tok0_md,
                        token_id = 0).run(sender = admin)

    scenario.h3("Create the auction")
    scenario += auction_house.create_auction(
        auction_and_token_id=sp.nat(0),
        end_timestamp=sp.timestamp(0).add_days(7),  
        voter_amount=sp.nat(3),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    scenario.h3("Add the 3 votes for the token")
    voter_money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address, dan.address], 
        auction_and_token_id=sp.nat(0),
    )).run(sender=admin)

    scenario.h3("end the auction before anyone has bid on it")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    scenario.h3("transfer the item to bob from alice so we can see that she actually got it")
    fa2.transfer(
    [
        BatchTransfer.item(from_ = alice.address,
                            txs = [
                                sp.record(to_ = bob.address,
                                          amount = 1,
                                          token_id = 0)
                            ])
    ]).run(sender = alice)

    scenario.h3("Alice can not withdraw because the auction got resolved but did not get bid on")
    voter_money_pool.withdraw().run(sender=alice, valid=False)

    tok1_md = TestHelper.make_metadata(
        name = "The Token Zero",
        decimals = 0,
        symbol= "TK1" )
    scenario.h3("Mint a new token")
    fa2.mint(address = auction_house.address,
                        amount = 1,
                        metadata = tok1_md,
                        token_id = 1).run(sender = admin)

    scenario.h3("create another auction with a new token")
    scenario += auction_house.create_auction(
        auction_and_token_id=sp.nat(1),
        end_timestamp=sp.timestamp(0).add_days(14),  #has to be like 2 weeks later
        voter_amount=sp.nat(3),
        uploader=alice.address,
        bid_amount=sp.mutez(1000000),
    ).run(sender=admin)

    scenario.h3("add the votes for the newly created auction")
    voter_money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address, dan.address], 
        auction_and_token_id=sp.nat(1),
    )).run(sender=admin)

    scenario.h3("dan bids")
    scenario += auction_house.bid(1).run(sender=dan,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(1))

    scenario.h3("end the auction")
    scenario += auction_house.end_auction(1).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(14))
    
    scenario.verify(auction_house.balance  == sp.mutez(0))

    scenario.verify(voter_money_pool.balance == sp.mutez(450000))
    voter_money_pool.withdraw().run(sender=alice)
    scenario.verify(voter_money_pool.balance == sp.mutez(300000))
    voter_money_pool.withdraw().run(sender=bob)
    scenario.verify(voter_money_pool.balance == sp.mutez(150000))
    voter_money_pool.withdraw().run(sender=dan)
    scenario.verify(voter_money_pool.balance == sp.mutez(0))