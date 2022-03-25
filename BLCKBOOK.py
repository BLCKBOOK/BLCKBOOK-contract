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
            "authors": ["Niels Hanselmann", "Simon Schiebler"],
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

    @sp.onchain_view(pure = True)
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

    @sp.onchain_view(pure = True)
    def count_tokens(self):
        """Get how many tokens are in this FA2 contract."""
        sp.result(self.data.all_tokens)

    @sp.onchain_view(pure = True)
    def does_token_exist(self, tok):
        "Ask whether a token ID is exists."
        sp.set_type(tok, sp.TNat)
        sp.result(self.data.token_metadata.contains(tok))

    @sp.onchain_view(pure = True)
    def all_tokens(self):
        sp.result(sp.range(0, self.data.all_tokens))

    @sp.onchain_view(pure = True)
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

        list_of_views = [
            self.get_expired_auctions
        ]

        metadata = {
            "name": "BLCKBOOK-Auction-House",
            "description": "BLCKBOOK beta implementation of the Auction-House",
            "views": list_of_views,
            "authors": ["Niels Hanselmann", "Simon Schiebler"],
            "homepage": "https://blckbook.vote",
            "source": {"tools": ["SmartPy"], "location": "https://github.com/BLCKBOOK/BLCKBOOK-contract"},
        }

        self.init_metadata("AuctionHouseContract", metadata)

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
                metadata = sp.TBigMap(sp.TString, sp.TBytes),
        ).layout(("administrator", ("blckbook_collector", ("voter_money_pool", ("tokens_contract_address", ("blckbook_share", ("uploader_share", ("voter_share", ("all_auctions", ("auctions", "metadata")))))))))))

        self.init(blckbook_share = sp.nat(25),
                    voter_share = sp.nat(15),
                    uploader_share = sp.nat(60),
                    auctions=sp.big_map(tkey=sp.TNat, tvalue = Auction.get_type()),
                    blckbook_collector = blckbook_collector,
                    administrator = administrator,
                    tokens_contract_address = tokens_contract_address,
                    voter_money_pool = voter_money_pool,
                    metadata = sp.big_map(tkey = sp.TString, tvalue = sp.TBytes),
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
    def set_metadata(self, params):
        sp.verify(sp.sender == self.data.administrator, VoterMoneyPoolErrorMessage.NOT_ADMIN)
        self.data.metadata[params.k] = params.v

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

    @sp.onchain_view(pure = True)
    def get_expired_auctions(self, timestamp):
        sp.set_type_expr(timestamp, sp.TTimestamp)
        i = sp.local('i', sp.nat(0))
        expired_auctions = sp.local('expired_auctions', sp.list([], t = sp.TNat))
        sp.while i.value < self.data.all_auctions:
            sp.if self.data.auctions.contains(i.value) & (timestamp > self.data.auctions[i.value].end_timestamp):
                expired_auctions.value.push(i.value)
            i.value += 1
        sp.result(expired_auctions.value)

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
        list_of_views = [
            self.get_balance
        ]

        metadata = {
            "name": "BLCKBOOK-VoterMoneyPool",
            "description": "BLCKBOOK beta implementation of a VoterMoneyPool",
            "views": list_of_views,
            "authors": ["Niels Hanselmann", "Simon Schiebler"],
            "homepage": "https://blckbook.vote",
            "source": {"tools": ["SmartPy"], "location": "https://github.com/BLCKBOOK/BLCKBOOK-contract"},
        }

        # Helper method that builds the metadata and produces the JSON representation as an artifact.
        self.init_metadata("VoterMoneyPoolContract", metadata) #the string is just for the output of the online-IDE


        sp.set_type_expr(administrator, sp.TAddress)
        self.init_type(sp.TRecord(
                administrator = sp.TAddress,
                auctions = sp.TBigMap(sp.TNat, sp.TMutez),
                vote_map = sp.TBigMap(sp.TAddress, sp.TList(sp.TNat)),
                metadata = sp.TBigMap(sp.TString, sp.TBytes),
        ).layout(("administrator", ("auctions", ("vote_map", "metadata")))))

        self.init(
            administrator = administrator,
            auctions=sp.big_map(tkey=sp.TNat, tvalue = sp.TMutez),
            vote_map = sp.big_map(tkey=sp.TAddress, tvalue=sp.TList(sp.TNat)),
            metadata = sp.big_map(tkey = sp.TString, tvalue = sp.TBytes),
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
        sp.verify(sp.sender == self.data.administrator, VoterMoneyPoolErrorMessage.NOT_ADMIN) # only admin can create auction (nft needs to be minted for auction-contract)
        sp.set_type_expr(votes, AddVotesParams.get_type())
        sp.for vote in votes.voter_addresses:
            self.data.vote_map[vote] = sp.cons(votes.auction_and_token_id, self.data.vote_map.get(vote, default_value = []))

    @sp.entry_point
    def withdraw(self):
        sp.verify(self.data.vote_map.contains(sp.sender), VoterMoneyPoolErrorMessage.NOT_A_VOTER)
        sum = sp.local("sum", sp.mutez(0))
        not_resolved_yet = sp.local('not_resolved_yet', sp.list([], t = sp.TNat))
        already_resolved = sp.local('already_resolved', sp.set({}, t = sp.TNat))
        sp.for auction in self.data.vote_map[sp.sender]:
            sp.if self.data.auctions.contains(auction) & ~(already_resolved.value.contains(auction)):
                sum.value = sum.value + self.data.auctions[auction]
                already_resolved.value.add(auction)
            sp.else:
                not_resolved_yet.value.push(auction)

        self.data.vote_map[sp.sender] = not_resolved_yet.value

        sp.if (sum.value > sp.mutez(0)):
            sp.send(sp.sender, sum.value)
        sp.else:
            sp.failwith(VoterMoneyPoolErrorMessage.ALL_VOTES_ALREADY_PAYED_OUT)

    @sp.entry_point
    def set_metadata(self, params):
        sp.verify(sp.sender == self.data.administrator, VoterMoneyPoolErrorMessage.NOT_ADMIN)
        self.data.metadata[params.k] = params.v

    @sp.onchain_view(pure = True)
    def get_balance(self, address):
        """This view calculates how much a voter will get from withdrawing"""
        sp.set_type(address, sp.TAddress)
        sum = sp.local("sum", sp.mutez(0))
        already_resolved = sp.local('already_resolved', sp.set({}, t = sp.TNat))
        sp.if self.data.vote_map.contains(address):
            sp.for auction in self.data.vote_map[address]:
                sp.if self.data.auctions.contains(auction) & ~(already_resolved.value.contains(auction)):
                    sum.value = sum.value + self.data.auctions[auction]
                    already_resolved.value.add(auction)
        sp.result(sum.value)

class FA2Spray(sp.Contract):
    """Minimal FA2 contract for fungible tokens.

    This is a minimal example showing how to implement an NFT following
    the FA2 standard in SmartPy. It is for illustrative purposes only.
    For a more flexible toolbox aimed at real world applications please
    refer to FA2_lib.
    """

    def __init__(self, administrator, the_vote, metadata_base, metadata_url):
        self.init(
            administrator=administrator,
            the_vote=the_vote,
            ledger=sp.big_map(tkey=sp.TPair(sp.TAddress, sp.TNat), tvalue=sp.TNat),
            metadata=sp.utils.metadata_of_url(metadata_url),
            next_token_id=sp.nat(0),
            operators=sp.big_map(
                tkey=sp.TRecord(
                    owner=sp.TAddress, operator=sp.TAddress, token_id=sp.TNat
                ).layout(("owner", ("operator", "token_id"))),
                tvalue=sp.TUnit,
            ),
            supply=sp.big_map(tkey=sp.TNat, tvalue=sp.TNat),
            token_metadata=sp.big_map(
                tkey=sp.TNat,
                tvalue=sp.TRecord(
                    token_id=sp.TNat, token_info=sp.TMap(sp.TString, sp.TBytes)
                ),
            ),
        )
        metadata_base["views"] = [
            self.all_tokens,
            self.get_balance,
            self.is_operator,
            self.total_supply,
        ]
        self.init_metadata("metadata_base", metadata_base)

    @sp.entry_point
    def transfer(self, batch):
        """Accept a list of transfer operations.

        Each transfer operation specifies a source: `from_` and a list
        of transactions. Each transaction specifies the destination: `to_`,
        the `token_id` and the `amount` to be transferred.

        Args:
            batch: List of transfer operations.
        Raises:
            `FA2_TOKEN_UNDEFINED`, `FA2_NOT_OPERATOR`, `FA2_INSUFFICIENT_BALANCE`
        """
        sp.set_type(batch, BatchTransfer.get_type())
        with sp.for_("transfer", batch) as transfer:
            with sp.for_("tx", transfer.txs) as tx:
                sp.verify(tx.token_id < self.data.next_token_id, "FA2_TOKEN_UNDEFINED")
                from_ = (transfer.from_, tx.token_id)
                to_ = (tx.to_, tx.token_id)
                sp.verify(
                    (transfer.from_ == sp.sender)
                    | self.data.operators.contains(
                        sp.record(
                            owner=transfer.from_,
                            operator=sp.sender,
                            token_id=tx.token_id,
                        ))
                    | (self.data.the_vote == sp.sender), # the vote can transfer anything
                    "FA2_NOT_OPERATOR",
                )
                self.data.ledger[from_] = sp.as_nat(
                    self.data.ledger.get(from_, 0) - tx.amount,
                    "FA2_INSUFFICIENT_BALANCE",
                )
                self.data.ledger[to_] = self.data.ledger.get(to_, 0) + tx.amount

    @sp.entry_point
    def update_operators(self, actions):
        """Accept a list of variants to add or remove operators.

        Operators can perform transfer on behalf of the owner.
        Owner is a Tezos address which can hold tokens.

        Only the owner can change its operators.

        Args:
            actions: List of operator update actions.
        Raises:
            `FA2_NOT_OWNER`
        """
        with sp.for_("update", actions) as action:
            with action.match_cases() as arg:
                with arg.match("add_operator") as operator:
                    sp.verify(operator.owner == sp.sender, "FA2_NOT_OWNER")
                    self.data.operators[operator] = sp.unit
                with arg.match("remove_operator") as operator:
                    sp.verify(operator.owner == sp.sender, "FA2_NOT_OWNER")
                    del self.data.operators[operator]

    @sp.entry_point
    def balance_of(self, callback, requests):
        """Send the balance of multiple account / token pairs to a
        callback address.

        transfer 0 mutez to `callback` with corresponding response.

        Args:
            callback (contract): Where we callback the answer.
            requests: List of requested balances.
        Raises:
            `FA2_TOKEN_UNDEFINED`, `FA2_CALLBACK_NOT_FOUND`
        """

        def f_process_request(req):
            sp.verify(req.token_id < self.data.next_token_id, "FA2_TOKEN_UNDEFINED")
            sp.result(
                sp.record(
                    request=sp.record(owner=req.owner, token_id=req.token_id),
                    balance=self.data.ledger.get((req.owner, req.token_id), 0),
                )
            )

        t_request = sp.TRecord(owner=sp.TAddress, token_id=sp.TNat)
        sp.set_type(requests, sp.TList(t_request))
        sp.set_type(
            callback,
            sp.TContract(sp.TList(sp.TRecord(request=t_request, balance=sp.TNat))),
        )
        sp.transfer(requests.map(f_process_request), sp.mutez(0), callback)

    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, "FA2_NOT_ADMIN")
        self.data.administrator = params

    @sp.entry_point
    def mint(self, to_, amount, token):
        """(Admin only) Create new tokens from scratch and assign
        them to `to_`.

        If `token` is "existing": increase the supply of the `token_id`.
        If `token` is "new": create a new token and assign the `metadata`.

        Args:
            to_ (address): Receiver of the tokens.
            amount (nat): Amount of token to be minted.
            token (variant): "_new_": id of the token, "_existing_": metadata of the token.
        Raises:
            `FA2_NOT_ADMIN`, `FA2_TOKEN_UNDEFINED`
        """
        sp.verify(sp.sender == self.data.administrator, "FA2_NOT_ADMIN")
        with token.match_cases() as arg:
            with arg.match("new") as metadata:
                sp.verify(self.data.next_token_id == 0, "FA2_NOT_SINGLE_ASSET")
                token_id = sp.compute(self.data.next_token_id) # only can mint one token and then create more of it
                self.data.token_metadata[token_id] = sp.record(
                    token_id=token_id, token_info=metadata
                )
                self.data.supply[token_id] = amount
                self.data.ledger[(to_, token_id)] = amount
                self.data.next_token_id += 1
            with arg.match("existing") as token_id:
                sp.verify(token_id < self.data.next_token_id, "FA2_TOKEN_UNDEFINED")
                self.data.supply[token_id] += amount
                self.data.ledger[(to_, token_id)] = (
                    self.data.ledger.get((to_, token_id), 0) + amount
                )

    @sp.onchain_view(pure=True)
    def all_tokens(self):
        """(Onchain view) Return the list of all the `token_id` known to the contract."""
        sp.result(sp.range(0, self.data.next_token_id))

    @sp.onchain_view(pure=True)
    def get_balance(self, params):
        """(Onchain view) Return the balance of an address for the specified `token_id`."""
        sp.set_type_expr(
            params,
            sp.TRecord(owner=sp.TAddress, token_id=sp.TNat).layout(
                ("owner", "token_id")
            ),
        )
        sp.verify(params.token_id < self.data.next_token_id, "FA2_TOKEN_UNDEFINED")
        sp.result(self.data.ledger.get((params.owner, params.token_id), 0))

    @sp.onchain_view(pure=True)
    def total_supply(self, params):
        """(Onchain view) Return the total number of tokens for the given `token_id` if known or
        fail if not."""
        sp.verify(params.token_id < self.data.next_token_id, "FA2_TOKEN_UNDEFINED")
        sp.result(self.data.supply.get(params.token_id, 0))

    @sp.onchain_view(pure=True)
    def is_operator(self, params):
        """(Onchain view) Return whether `operator` is allowed to transfer `token_id` tokens
        owned by `owner`."""
        sp.result(self.data.operators.contains(params))


metadata_base = {
    "version": "1.0.0",
    "description": "This is an adapted  minimal implementation of FA2 (TZIP-012) using SmartPy. It is used for the $PRAY-Token",
    "interfaces": ["TZIP-012", "TZIP-016"],
    "authors": ["SmartPy <https://smartpy.io/#contact>"],
    "homepage": "https://smartpy.io/ide?template=fa2_fungible_minimal.py",
    "source": {
        "tools": ["SmartPy"],
        "location": "https://gitlab.com/SmartPy/smartpy/-/raw/master/python/templates/fa2_fungible_minimal.py",
    },
    "permissions": {
        "operator": "owner-or-operator-transfer",
        "receiver": "owner-no-hook",
        "sender": "owner-no-hook",
    },
}

class TheVote(sp.Contract):
    def __init__(self, administrator, tokens_contract_address, auction_house_address, voter_money_pool_address):
        sp.set_type_expr(administrator, sp.TAddress)
        sp.set_type_expr(tokens_contract_address, sp.TAddress)
        sp.set_type_expr(auction_house_address, sp.TAddress)
        sp.set_type_expr(voter_money_pool_address, sp.TAddress)

        list_of_views = [
        ]

        metadata = {
            "name": "BLCKBOOK The Vote",
            "description": "BLCKBOOK The Vote beta implementation. Using SmartPy.\n\n",
            "version": "0.1",
            "views": list_of_views,
            "interfaces": [],
            "authors": ["Niels Hanselmann"],
            "homepage": "https://blckbook.vote",
            "source": {"tools": ["SmartPy"], "location": "https://github.com/BLCKBOOK/BLCKBOOK-contract"},
            "permissions": {},
        }

        # Helper method that builds the metadata and produces the JSON representation as an artifact.
        self.init_metadata("BLCKBOOK-THE-VOTE", metadata)  # the string is just for the output of the online-IDE

        self.init_type(sp.TRecord(
            administrator=sp.TAddress,
            tokens_contract_address=sp.TAddress,
            spray_contract_address=sp.TAddress,
            auction_house_address=sp.TAddress,
            voter_money_pool_address=sp.TAddress,
            all_artworks=sp.TNat,
            metadata=sp.TBigMap(sp.TString, sp.TBytes),
            artwork_data=sp.TBigMap(sp.TNat, sp.TRecord(artwork_id=sp.TNat, artwork_info=sp.TMap(sp.TString, sp.TBytes), uploader=sp.TAddress)),
            vote_count=sp.TMap(sp.TNat, sp.TNat),
            vote_register=sp.TBigMap(sp.TNat, sp.TSet(sp.TAddress)),
            is_sorted=sp.TBool,
            minting_started=sp.TBool,
            artworks_to_mint=sp.TSet(sp.TNat),
            admission_deadline=sp.TTimestamp,
            divisor=sp.TNat,
        ))
        self.init(
            administrator=administrator,
            tokens_contract_address=tokens_contract_address,
            spray_contract_address=tokens_contract_address,
            auction_house_address=auction_house_address,
            voter_money_pool_address=voter_money_pool_address,
            all_artworks=0,
            metadata=sp.big_map(tkey=sp.TString, tvalue=sp.TBytes),
            artwork_data=sp.big_map(
                tkey=sp.TNat,
                tvalue=sp.TRecord(artwork_id=sp.TNat, artwork_info=sp.TMap(sp.TString, sp.TBytes), uploader=sp.TAddress)
            ),
            # if vote_count is set to 0 for an artwork we say that it is approved.
            # Approval is done either off-chain or it can be done-on-chain if somebody submitted on-chain
            vote_count=sp.map(tkey = sp.TNat, tvalue = sp.TNat),
            vote_register=sp.big_map(tkey=sp.TNat, tvalue=sp.TSet(sp.TAddress)),
            is_sorted=False,
            minting_started=False,
            artworks_to_mint=sp.set({}, t= sp.TNat),
            admission_deadline = sp.now.add_days(7),
            divisor=sp.nat(10)
            # after origination, we have a week to admit artworks
        )

    @sp.entry_point
    def set_administrator(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        self.data.administrator = params

    @sp.entry_point
    def set_spray_contract(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        self.data.spray_contract_address = params

    @sp.entry_point
    def set_divisor(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        sp.verify(params > 0, "THE_VOTE_DIVISOR_MUST_BE_GREATER_THAN_0")
        self.data.divisor = params

    @sp.entry_point
    def set_administrator_of_token_contract(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        sp.set_type_expr(params, sp.TAddress)

        token_contract = sp.contract(sp.TAddress, self.data.tokens_contract_address,
                                                entry_point="set_administrator").open_some()
        sp.transfer(
            params,
            sp.mutez(0),
            token_contract,
        )

    @sp.entry_point
    def set_administrator_of_voter_money_pool_contract(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        sp.set_type_expr(params, sp.TAddress)

        voter_money_pool_contract = sp.contract(sp.TAddress, self.data.voter_money_pool_address,
                                     entry_point="set_administrator").open_some()
        sp.transfer(
            params,
            sp.mutez(0),
            voter_money_pool_contract,
        )

    @sp.entry_point
    def set_administrator_of_auction_house_contract(self, params):
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        sp.set_type_expr(params, sp.TAddress)

        auction_house_contract = sp.contract(sp.TAddress, self.data.auction_house_address,
                                     entry_point="set_administrator").open_some()
        sp.transfer(
            params,
            sp.mutez(0),
            auction_house_contract,
        )

    @sp.entry_point
    def set_tokens_contract_address(self, params):
        """
        Entry-Point for setting the FA2-Contract Address
        """
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        self.data.tokens_contract_address = params

    @sp.entry_point
    def set_voter_money_pool_address(self, params):
        """
        Entry-Point for setting the address of the voter_money_pool which will get the shares for the voters and will get called with the info how much every voter gets
        """
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        self.data.voter_money_pool_address = params

    @sp.entry_point
    def admission(self, metadata, uploader):
        """
        Add an artwork so it can be voted for in the next cycle. Only admin can do this
        """
        sp.verify(sp.sender == self.data.administrator, 'THE_VOTE_NOT_ADMIN')
        sp.verify(sp.now < self.data.admission_deadline, "THE_VOTE_ADMISSION_DEADLINE_PASSED")

        sp.set_type_expr(metadata, sp.TMap(sp.TString, sp.TBytes))
        sp.set_type_expr(uploader, sp.TAddress)

        self.data.artwork_data[self.data.all_artworks] = sp.record(artwork_id = self.data.all_artworks, artwork_info = metadata, uploader = uploader)
        self.data.vote_count[self.data.all_artworks] = 0
        self.data.vote_register[self.data.all_artworks] = sp.set([])
        self.data.all_artworks = self.data.all_artworks + 1

    @sp.entry_point
    def vote(self, artwork_id, amount):
        """
        Add votes to an artwork. Will get the amount of $PRAY tokens and transmit them to THE VOTE
        """
        sp.set_type_expr(artwork_id, sp.TNat)
        sp.set_type_expr(amount, sp.TNat)
        sp.verify(amount > 0, 'THE_VOTE_AMOUNT_IS_ZERO')
        sp.verify(~self.data.is_sorted, "THE_VOTE_VOTES_ARE_ALREADY_SORTED")

        spray_contract = sp.contract(BatchTransfer.get_type(), self.data.spray_contract_address,
                                     entry_point="transfer").open_some()

        # we now transfer the $PRAY tokens to the contract itself
        sp.transfer([BatchTransfer.item(sp.sender, [
            sp.record(to_ = sp.self_address, token_id = 0, amount = amount)])],
                    sp.mutez(0), spray_contract)

        self.data.vote_count[artwork_id] += amount
        self.data.vote_register[artwork_id].add(sp.sender)

    @sp.entry_point
    def provide_sorted_votes(self, sorted_list):
        """
        Provide a sorted_list of the vote_count map. Can only be called after the admission timeline has passed
        This will is a mandatory step before minting the succesful votes
        This will determine which artworks are successful
        """

        sp.verify(sp.now > self.data.admission_deadline, "THE_VOTE_ADMISSION_HAS_NOT_PASSED")
        sp.verify(~self.data.minting_started, "THE_VOTE_MINTING_ALREADY_STARTED")

        sp.set_type_expr(sorted_list, sp.TList(sp.TRecord(id = sp.TNat,
                             vote_amount = sp.TNat)))

        sp.verify(sp.len(sorted_list) == sp.len(self.data.vote_count), "THE_VOTE_WRONG_LENGTH")
        already_sorted = sp.local('already_sorted', sp.set({}, t=sp.TNat))

        current_index = sp.local("current_index", sp.nat(0))
        quotient = sp.local("quotient", sp.len(sorted_list) // self.data.divisor)

        self.data.artworks_to_mint = sp.set({}, t=sp.TNat)

        with sp.match_cons(sorted_list) as x1:
            last_entry = sp.local("last_entry", x1.head.vote_amount)
            sp.for sorted_entry in sorted_list:
                current_count = sp.local("current_count", self.data.vote_count.get(sorted_entry.id, None, "THE_VOTE_WRONG_ID"))
                sp.if ~(current_count.value == sorted_entry.vote_amount):
                    sp.failwith("THE_VOTE_NOT_THE_SAME_AMOUNT")
                sp.if sorted_entry.vote_amount <= last_entry.value:
                    last_entry.value = current_count.value
                sp.else:
                    sp.failwith("THE_VOTE_NOT_SORTED_LIST")
                sp.if already_sorted.value.contains(sorted_entry.id):
                    sp.failwith("THE_artwork_id_ALREADY_SEEN")
                sp.else:
                    already_sorted.value.add(sorted_entry.id)
                sp.if current_index.value <= quotient.value:
                    self.data.artworks_to_mint.add(sorted_entry.id)
                current_index.value += 1

        sp.else:
            sp.if ~(sp.len(sorted_list) == 0):
                sp.failwith("THE_VOTE_HEAD_MATCHING_WRONG")

        self.data.is_sorted = True

    @sp.entry_point
    def mint_artworks(self, max_amount):
        """
        Ends a voting-period
        """

        sp.verify(self.data.is_sorted, "THE_VOTE_VOTES_ARE_NOT_SORTED")

        self.data.minting_started = True
        sp.set_type_expr(max_amount, sp.TNat)

        current_index = sp.local("current_index", sp.nat(0))

        token_index = sp.local("token_index", sp.view("count_tokens", self.data.tokens_contract_address, sp.unit, sp.TNat).open_some("THE_VOTE_INVALID_VIEW"))

        sp.for artwork in self.data.artworks_to_mint.elements():
            sp.if (current_index.value < max_amount):
                token_contract = sp.contract(
                    sp.TRecord(amount = sp.TNat, token_id = sp.TNat, address = sp.TAddress, metadata = sp.TMap(sp.TString, sp.TBytes)),
                    self.data.tokens_contract_address,
                    entry_point="mint").open_some()

                sp.transfer(sp.record(amount= sp.nat(1),
                                      token_id = current_index.value + token_index.value,
                                      address=self.data.auction_house_address,
                                      metadata= self.data.artwork_data[artwork].artwork_info),
                    sp.mutez(0),
                    token_contract,
                )

                auction_house_contract = sp.contract(
                    sp.TRecord(auction_and_token_id = sp.TNat, end_timestamp = sp.TTimestamp, voter_amount = sp.TNat, uploader = sp.TAddress, bid_amount=sp.TMutez),
                    self.data.auction_house_address,
                    entry_point="create_auction").open_some()

                sp.transfer(
                    sp.record(auction_and_token_id = current_index.value + token_index.value,
                              end_timestamp = sp.now.add_days(7),
                              voter_amount = sp.len(self.data.vote_register.get(artwork).elements()),
                              uploader = self.data.artwork_data[artwork].uploader,
                              bid_amount=sp.mutez(1000000)),
                    sp.mutez(0),
                    auction_house_contract,
                )

                voter_money_pool_contract = sp.contract(sp.TRecord(voter_addresses=sp.TList(sp.TAddress), auction_and_token_id=sp.TNat), self.data.voter_money_pool_address,
                                                        entry_point="add_votes").open_some()
                sp.transfer(
                    sp.record(voter_addresses=self.data.vote_register[artwork].elements(), auction_and_token_id= current_index.value + token_index.value),
                    sp.mutez(0),
                    voter_money_pool_contract,
                )

                current_index.value += 1

                del self.data.artwork_data[artwork]
                del self.data.vote_register[artwork]
                self.data.artworks_to_mint.remove(artwork)

        sp.if sp.len(self.data.artworks_to_mint.elements()) == 0:
            # this means we minted all or there was nothing to mint, so we now reset everything
            self.data.admission_deadline = sp.now.add_days(7)
            self.data.is_sorted = False
            self.data.minting_started = False

            # we delete the vote_count and also the vote_register as we transmitted them for the winning artworks
            sp.for vote_to_delete in self.data.vote_count.items():
                del self.data.vote_count[vote_to_delete.key]
                del self.data.vote_register[vote_to_delete.key]

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

"""
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

    scenario.h2("View should return an empty array for an empty contract")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(1)))

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

    scenario.h3("View is still empty")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(1)))

    scenario.h3("Bob bids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(2000000), now=sp.timestamp(0).add_minutes(1))

    scenario.h3("bob tries to bid a increase that is too small")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(2000001), now=sp.timestamp(0).add_minutes(2), valid=False)

    scenario.h3("dan bids again (with a higher sum)")
    scenario += auction_house.bid(0).run(sender=dan,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(3))

    scenario.h3("Bob rebids")
    scenario += auction_house.bid(0).run(sender=bob,amount=sp.mutez(401001327), now=sp.timestamp(0).add_minutes(4))

    scenario.h3("View is still empty")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(5)))
    scenario.h3("Admin can not end a running auction")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(8), valid=False)

    scenario.h3("Alice can't bid cause she is the uploader")
    scenario += auction_house.bid(0).run(sender=alice,amount=sp.mutez(3000000), now=sp.timestamp(0).add_minutes(10), valid=False)

    scenario.h2("Try to bid on auctions, that is over")
    scenario += auction_house.bid(0).run(sender=dan, amount=sp.mutez(501001327), now=sp.timestamp(0).add_minutes(5).add_days(7), valid=False)

    scenario.h3("View is not empty anymore")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(5).add_days(7)))

    scenario.h2("Admin ends auction")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    scenario.h3("View is not empty again")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(7).add_days(7)))
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

    scenario.h3("test offchain-view get-balance when the voter has not voted for anything")
    scenario.verify(voter_money_pool.get_balance(alice.address) == sp.mutez(0))

    voter_money_pool.add_votes(sp.record(
            voter_addresses=[alice.address, bob.address],
            auction_and_token_id=sp.nat(0),
        )).run(sender=admin)

    scenario.h3("Alice should not be able to withdraw before the reward gets set")
    voter_money_pool.withdraw().run(sender=alice, valid=False)

    scenario.h3("test offchain-view get-balance is 0 for alice before setting the voter reward")
    scenario.verify(voter_money_pool.get_balance(alice.address) == sp.mutez(0))

    scenario.h3("Admin should be able to set voter rewards")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(0), reward=sp.mutez(200)).run(sender=admin, amount=sp.mutez(400))

    scenario.h3("Admin should not be able to set voter rewards for the same auction twice")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(0), reward=sp.mutez(200)).run(sender=admin, amount=sp.mutez(400), valid=False)

    scenario.h3("test offchain-view get-balance with amount")
    scenario.verify(voter_money_pool.get_balance(alice.address) == sp.mutez(200))
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


    scenario.h1("Check what happens when the admin messes up and enters wrong data")
    scenario.h3("So we add the same voters twice")
    voter_money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address],
        auction_and_token_id=sp.nat(5),
    )).run(sender=admin)

    voter_money_pool.add_votes(sp.record(
        voter_addresses=[alice.address, bob.address],
        auction_and_token_id=sp.nat(5),
    )).run(sender=admin)

    scenario.h3("Now we resolve the vote_pool")
    voter_money_pool.set_auction_rewards(auction_and_token_id=sp.nat(5), reward=sp.mutez(400)).run(sender=admin, amount=sp.mutez(800))

    scenario.h3("And the withdrawls (including the view) should return the right values and not count something twice")
    scenario.verify(voter_money_pool.get_balance(alice.address) == sp.mutez(400))
    voter_money_pool.withdraw().run(sender=alice, valid=True)
    scenario.verify(voter_money_pool.balance  == sp.mutez(400))
    voter_money_pool.withdraw().run(sender=bob, valid=True)
    scenario.verify(voter_money_pool.balance  == sp.mutez(0))

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

    scenario.h3("View is not empty")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(5).add_days(7)))

    scenario.h3("end the auction before anyone has bid on it")
    scenario += auction_house.end_auction(0).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(7))

    scenario.h3("View is empty")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(6).add_days(7)))

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

    scenario.h3("View has 1 in it")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(4).add_days(14)))

    scenario.h3("end the auction")
    scenario += auction_house.end_auction(1).run(sender=admin, amount=sp.mutez(0), now=sp.timestamp(0).add_minutes(5).add_days(14))

    scenario.h3("View is empty again")
    scenario.show(auction_house.get_expired_auctions(sp.timestamp(0).add_minutes(6).add_days(14)))

    scenario.verify(auction_house.balance  == sp.mutez(0))

    scenario.verify(voter_money_pool.balance == sp.mutez(450000))
    voter_money_pool.withdraw().run(sender=alice)
    scenario.verify(voter_money_pool.balance == sp.mutez(300000))
    voter_money_pool.withdraw().run(sender=bob)
    scenario.verify(voter_money_pool.balance == sp.mutez(150000))
    voter_money_pool.withdraw().run(sender=dan)
    scenario.verify(voter_money_pool.balance == sp.mutez(0))

@sp.add_test(name = "For Origination")
def test():

    admin_address = sp.address("tz1PEbaFp9jE6syH5xg29YRegbwLLehzK3w2")
    scenario = sp.test_scenario()
    scenario.h1("For origination")
    scenario.table_of_contents()

    fa2 = TokensContract(admin_address)
    scenario += fa2

    voter_money_pool = VoterMoneyPoolContract(admin_address)
    scenario += voter_money_pool

    auction_house = AuctionHouseContract(administrator=admin_address,
        voter_money_pool = sp.address('KT1XeA6tZYeBCm7aux3SAPswTuRE72R3VUCW'),
        blckbook_collector = admin_address,
        tokens_contract_address = sp.address('KT1HAtdXKvXqK2He3Xr2xmHQ9cYrxPTL7X9Z'))
    scenario += auction_house

    the_vote = TheVote(administrator=admin_address,
        tokens_contract_address = sp.address('KT1HAtdXKvXqK2He3Xr2xmHQ9cYrxPTL7X9Z'),
        auction_house_address=sp.address('KT1XeA6tZYeBCm7aux3SAPswTuRE72R3VUCW'),
        voter_money_pool_address=sp.address('KT1XeA6tZYeBCm7aux3SAPswTuRE72R3VUCW'))
    scenario += the_vote

"""
@sp.add_test(name = "Setting Admins of other contracts through theVote")
def test():

    scenario = sp.test_scenario()
    scenario.h1("Setting Admins of other contracts through theVote")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    admin_address = admin.address;

    fa2 = TokensContract(admin_address)
    scenario += fa2

    voter_money_pool = VoterMoneyPoolContract(admin_address)
    scenario += voter_money_pool

    auction_house = AuctionHouseContract(administrator=admin_address,
        voter_money_pool = voter_money_pool.address,
        blckbook_collector = admin_address,
        tokens_contract_address = fa2.address)
    scenario += auction_house

    the_vote = TheVote(administrator=admin_address,
        tokens_contract_address = fa2.address,
        auction_house_address=auction_house.address,
        voter_money_pool_address=voter_money_pool.address)
    scenario += the_vote

    scenario.h2("Admin Sets for FA2")

    scenario.h3("Set theVote as admin of FA2")
    scenario += fa2.set_administrator(the_vote.address).run(sender=admin)
    scenario.h3("the Votes sets admin as admin of FA2 again through the nested call")
    scenario += the_vote.set_administrator_of_token_contract(admin.address).run(sender=admin)
    scenario.h3("Set theVote as admin of FA2 again")
    scenario += fa2.set_administrator(the_vote.address).run(sender=admin)

    scenario.h3("Bob can not set the admin of FA2 through theVote cause he is not its admin")
    bob = sp.test_account("Bob")
    scenario += the_vote.set_administrator_of_token_contract(admin.address).run(sender=bob, valid=False)

    scenario.h2("Admin Sets for Auction_House")

    scenario.h3("Set theVote as admin of Auction_house")
    scenario += auction_house.set_administrator(the_vote.address).run(sender=admin)
    scenario.h3("the Votes sets admin as admin of Auction_house again through the nested call")
    scenario += the_vote.set_administrator_of_auction_house_contract(admin.address).run(sender=admin)
    scenario.h3("Set theVote as admin of Auction_House again")
    scenario += auction_house.set_administrator(the_vote.address).run(sender=admin)

    scenario.h2("Admin Sets for Voter_Money_Pool_House")
    scenario.h3("Set theVote as admin of Voter_Money_Pool")
    scenario += voter_money_pool.set_administrator(the_vote.address).run(sender=admin)
    scenario.h3("the Votes sets admin as admin ofvoter_money_pool again through the nested call")
    scenario += the_vote.set_administrator_of_voter_money_pool_contract(admin.address).run(sender=admin)
    scenario.h3("Set theVote as admin of voter_money_pool again")
    scenario += voter_money_pool.set_administrator(the_vote.address).run(sender=admin)

@sp.add_test(name = "Simple Artwork Admission")
def test():

    scenario = sp.test_scenario()
    scenario.h1("Simple Artwork Admission")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    admin_address = admin.address;

    fa2 = TokensContract(admin_address)
    scenario += fa2

    voter_money_pool = VoterMoneyPoolContract(admin_address)
    scenario += voter_money_pool

    auction_house = AuctionHouseContract(administrator=admin_address,
        voter_money_pool = voter_money_pool.address,
        blckbook_collector = admin_address,
        tokens_contract_address = fa2.address)
    scenario += auction_house

    the_vote = TheVote(administrator=admin_address,
        tokens_contract_address = fa2.address,
        auction_house_address=auction_house.address,
        voter_money_pool_address=voter_money_pool.address)
    scenario += the_vote

    bob = sp.test_account("Bob")

    metadata = TestHelper.make_metadata(
        name = "Bobs Spot",
        decimals = 0,
        symbol= "TK0" )

    scenario.h2("Simple Artwork Admission")
    scenario.h3("Bob can submit artwork")
    scenario += the_vote.admission(metadata = metadata, uploader = bob.address).run(sender=bob, valid=False)
    scenario.h3("Admin can")
    scenario += the_vote.admission(metadata = metadata, uploader = bob.address).run(sender=admin, valid=True)


    fa2_admin = sp.test_account("fa2_admin")

@sp.add_test(name="FA2Spray Test")
def test():
    scenario = sp.test_scenario()
    admin = sp.test_account("Administrator")
    bob = sp.test_account("Bob")
    alice = sp.test_account("Alice")

    spray = FA2Spray(admin.address, admin.address, metadata_base, "https//example.com")
    scenario += spray

    spray_metadata = TestHelper.make_metadata(
        name="$PRAY",
        decimals=0,
        symbol="$PRAY")

    scenario.h2("Simple $PRAY Minting tests")
    scenario.h3("Bob can not mint a new token")
    spray.mint(to_= bob.address, amount = 1000, token = sp.variant("new", spray_metadata)).run(sender=bob, valid=False)
    scenario.h3("can mint the first $PRAY for alice")
    spray.mint(to_= alice.address, amount = 1000,token = sp.variant("new", spray_metadata)).run(sender=admin)
    scenario.h3("can not mint a new token for bob as alice")
    spray.mint(to_= bob.address, amount = 1000, token = sp.variant("new", spray_metadata)).run(sender=admin, valid=False)
    scenario.h3("But we can mint him existing ones")
    spray.mint(to_= bob.address, amount = 1000, token = sp.variant("existing", 0)).run(sender=admin)

@sp.add_test(name = "Vote Tests")
def test():

    scenario = sp.test_scenario()
    scenario.h1("Simple Artwork Admission")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    admin_address = admin.address;

    fa2 = TokensContract(admin_address)
    scenario += fa2

    voter_money_pool = VoterMoneyPoolContract(admin_address)
    scenario += voter_money_pool

    auction_house = AuctionHouseContract(administrator=admin_address,
        voter_money_pool = voter_money_pool.address,
        blckbook_collector = admin_address,
        tokens_contract_address = fa2.address)
    scenario += auction_house

    the_vote = TheVote(administrator=admin_address,
        tokens_contract_address = fa2.address,
        auction_house_address=auction_house.address,
        voter_money_pool_address=voter_money_pool.address)
    scenario += the_vote

    bob = sp.test_account("Bob")

    metadata = TestHelper.make_metadata(
        name = "Bobs Spot",
        decimals = 0,
        symbol= "TK0")

    scenario.h2("Vote Tests")
    scenario.h3("Admin submits artwork")
    scenario += the_vote.admission(metadata = metadata, uploader = bob.address).run(sender=admin, valid=True)

    spray = FA2Spray(admin.address, the_vote.address, metadata_base, "https//example.com")
    scenario += spray

    spray_metadata = TestHelper.make_metadata(
        name="$PRAY",
        decimals=0,
        symbol="$PRAY")

    the_vote.set_spray_contract(spray.address).run(sender=admin)

    scenario.h3("we mint the first $PRAY for bob")
    spray.mint(to_=bob.address, amount=1000, token=sp.variant("new", spray_metadata)).run(sender=admin)

    spray.transfer([BatchTransfer.item(bob.address, [
        sp.record(to_=admin.address, token_id=0, amount=300)])]).run(sender=bob)

    scenario.h3("bob votes for his own upload")
    the_vote.vote(artwork_id = 0, amount = 100).run(sender=bob)

    scenario.h3("bob tries to vote with too much")
    the_vote.vote(artwork_id = 0, amount = 1000).run(sender=bob, valid=False)

    scenario.h3("bob votes again with a small amount")
    the_vote.vote(artwork_id=0, amount=100).run(sender=bob)

    scenario.h3("bob votes with an amount of 0")
    the_vote.vote(artwork_id=0, amount=0).run(sender=bob, valid=False)
@sp.add_test(name = "Provide Sorted Vote List Test")
def test():

    scenario = sp.test_scenario()
    scenario.h1("Provide Sorted Vote List Test")
    scenario.table_of_contents()

    admin = sp.test_account("Administrator")
    admin_address = admin.address;

    fa2 = TokensContract(admin_address)
    scenario += fa2

    voter_money_pool = VoterMoneyPoolContract(admin_address)
    scenario += voter_money_pool

    auction_house = AuctionHouseContract(administrator=admin_address,
                                         voter_money_pool=voter_money_pool.address,
                                         blckbook_collector=admin_address,
                                         tokens_contract_address=fa2.address)
    scenario += auction_house

    the_vote = TheVote(administrator=admin_address,
                       tokens_contract_address=fa2.address,
                       auction_house_address=auction_house.address,
                       voter_money_pool_address=voter_money_pool.address)
    scenario += the_vote

    bob = sp.test_account("Bob")
    metadata = TestHelper.make_metadata(
        name = "Bobs Spot",
        decimals = 0,
        symbol= "TK0")

    scenario.h2("Vote Tests")
    scenario.h3("Admin submits 5 artworks")
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)

    spray = FA2Spray(admin.address, the_vote.address, metadata_base, "https//example.com")
    scenario += spray

    spray_metadata = TestHelper.make_metadata(
        name="$PRAY",
        decimals=0,
        symbol="$PRAY")

    the_vote.set_spray_contract(spray.address).run(sender=admin)

    scenario += fa2.set_administrator(the_vote.address).run(sender=admin)
    scenario += auction_house.set_administrator(the_vote.address).run(sender=admin)
    scenario += voter_money_pool.set_administrator(the_vote.address).run(sender=admin)

    scenario.h3("we mint the first $PRAY for bob")
    spray.mint(to_=bob.address, amount=1000, token=sp.variant("new", spray_metadata)).run(sender=admin)

    scenario.h3("we provide a lot of sorted votes to test the algorithm without having voted")
    scenario.h4("we provide a lot of sorted votes to test the algorithm without having voted")
    the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=0),
                                   sp.record(id=1, vote_amount=0),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=3, vote_amount=0),
                                   sp.record(id=4, vote_amount=0)]).run(sender=bob, valid=False)



    the_vote.provide_sorted_votes([sp.record(id=4, vote_amount=0),
                                   sp.record(id=0, vote_amount=0),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=1, vote_amount=0),
                                   sp.record(id=3, vote_amount=0)]).run(sender=bob, now = sp.timestamp(0).add_days(8))

    the_vote.provide_sorted_votes([sp.record(id=4, vote_amount=0),
                                   sp.record(id=0, vote_amount=0),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=1, vote_amount=0),
                                   sp.record(id=1, vote_amount=0),
                                   sp.record(id=3, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=4, vote_amount=0),
                                   sp.record(id=1, vote_amount=0),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=3, vote_amount=0),
                                   sp.record(id=4, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=0),
                                   sp.record(id=0, vote_amount=0),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=3, vote_amount=0),
                                   sp.record(id=4, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=2),
                                   sp.record(id=0, vote_amount=1),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=3, vote_amount=0),
                                   sp.record(id=4, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=2),
                                   sp.record(id=1, vote_amount=1),
                                   sp.record(id=2, vote_amount=0),
                                   sp.record(id=3, vote_amount=0),
                                   sp.record(id=4, vote_amount=0)]).run(sender=bob, valid=False)

    scenario.h3("now we mint the wining artwork, that did not get any votes")
    the_vote.mint_artworks(1).run(sender=bob)

    scenario.h3("now we bob can not vote on these artworks anymore")
    the_vote.vote(artwork_id=0, amount=1).run(sender=bob, valid=False)
    the_vote.vote(artwork_id=1, amount=1).run(sender=bob, valid=False)
    the_vote.vote(artwork_id=2, amount=10).run(sender=bob, valid=False)
    the_vote.vote(artwork_id=3, amount=10).run(sender=bob, valid=False)
    the_vote.vote(artwork_id=4, amount=10).run(sender=bob, valid=False)

    scenario.h3("So now we add new artworks")
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=True)
    
    scenario.h3("now bob votes on these new artworks")
    the_vote.vote(artwork_id=5, amount=1).run(sender=bob)
    the_vote.vote(artwork_id=6, amount=1).run(sender=bob)
    the_vote.vote(artwork_id=7, amount=12).run(sender=bob)
    the_vote.vote(artwork_id=8, amount=10).run(sender=bob)
    the_vote.vote(artwork_id=9, amount=10).run(sender=bob)


    scenario.h3("we now send different votes to test the algorithm")

    the_vote.provide_sorted_votes([sp.record(id=7, vote_amount=12),
                                   sp.record(id=9, vote_amount=10),
                                   sp.record(id=8, vote_amount=10),
                                   sp.record(id=6, vote_amount=1),
                                   sp.record(id=5, vote_amount=1),
                                   sp.record(id=10, vote_amount=0),
                                   sp.record(id=11, vote_amount=0),
                                   sp.record(id=12, vote_amount=0),
                                   sp.record(id=14, vote_amount=0),
                                   sp.record(id=13, vote_amount=0)]).run(sender=bob, now = sp.timestamp(0).add_days(16))

    the_vote.provide_sorted_votes([sp.record(id=7, vote_amount=12),
                                   sp.record(id=9, vote_amount=10),
                                   sp.record(id=9, vote_amount=10),
                                   sp.record(id=6, vote_amount=1),
                                   sp.record(id=5, vote_amount=1),
                                   sp.record(id=10, vote_amount=0),
                                   sp.record(id=11, vote_amount=0),
                                   sp.record(id=12, vote_amount=0),
                                   sp.record(id=14, vote_amount=0),
                                   sp.record(id=13, vote_amount=0)]).run(sender=bob, valid=False)

    the_vote.provide_sorted_votes([sp.record(id=7, vote_amount=12),
                                   sp.record(id=9, vote_amount=10),
                                   sp.record(id=8, vote_amount=10),
                                   sp.record(id=6, vote_amount=1),
                                   sp.record(id=5, vote_amount=1),
                                   sp.record(id=14, vote_amount=0),
                                   sp.record(id=11, vote_amount=0),
                                   sp.record(id=12, vote_amount=0),
                                   sp.record(id=10, vote_amount=0),
                                   sp.record(id=13, vote_amount=0)]).run(sender=bob)

    scenario.h3("now we mint the 2 NFTs")
    the_vote.mint_artworks(2).run(sender=bob)
    scenario.h3("Check that mint and providing sorted votes now do not work anymore")
    the_vote.mint_artworks(2).run(sender=bob, valid = False)

    the_vote.provide_sorted_votes([sp.record(id=7, vote_amount=12),
                                   sp.record(id=9, vote_amount=10),
                                   sp.record(id=8, vote_amount=10),
                                   sp.record(id=6, vote_amount=1),
                                   sp.record(id=5, vote_amount=1),
                                   sp.record(id=14, vote_amount=0),
                                   sp.record(id=11, vote_amount=0),
                                   sp.record(id=12, vote_amount=0),
                                   sp.record(id=10, vote_amount=0),
                                   sp.record(id=13, vote_amount=0)]).run(sender=bob, valid=False)

    @sp.add_test(name="Edgecase THE_VOTE No Artwork")
    def test():
        scenario = sp.test_scenario()
        scenario.h1("Edgecase THE_VOTE No Artwork")
        scenario.table_of_contents()

        admin = sp.test_account("Administrator")
        admin_address = admin.address;

        fa2 = TokensContract(admin_address)
        scenario += fa2

        voter_money_pool = VoterMoneyPoolContract(admin_address)
        scenario += voter_money_pool

        auction_house = AuctionHouseContract(administrator=admin_address,
                                             voter_money_pool=voter_money_pool.address,
                                             blckbook_collector=admin_address,
                                             tokens_contract_address=fa2.address)
        scenario += auction_house

        the_vote = TheVote(administrator=admin_address,
                           tokens_contract_address=fa2.address,
                           auction_house_address=auction_house.address,
                           voter_money_pool_address=voter_money_pool.address)
        scenario += the_vote

        bob = sp.test_account("Bob")

        scenario.h3("A late admission did not make it")
        scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin, valid=False, now = sp.timestamp(0).add_days(8))

        scenario.h3("Let the admission time begin without a single admission")
        the_vote.provide_sorted_votes([]).run(sender=admin, now = sp.timestamp(0).add_days(8))
        scenario += the_vote.mint_artworks(0).run(sender=admin)

        scenario.h3("Now the admission will make it")
        scenario += the_vote.admission(metadata=metadata, uploader=bob.address).run(sender=admin)
        the_vote.provide_sorted_votes([sp.record(id=0, vote_amount=0)]).run(sender=admin,  now = sp.timestamp(0).add_days(16))
        scenario += the_vote.mint_artworks(0).run(sender=admin)

    #
    # TODO:
    #  1. implement the $PRAY Bank (with tests)
    #  2. tests the interactions of all contracts with each other (from the vote to the payout of the auctions)