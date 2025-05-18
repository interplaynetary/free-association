-- CreateTable
CREATE TABLE "Node" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "type" TEXT NOT NULL,
    "points" INTEGER,
    "manual_fulfillment" REAL,
    "user_id" TEXT,
    "created_at" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updated_at" DATETIME NOT NULL,
    "parent_id" TEXT,
    CONSTRAINT "Node_parent_id_fkey" FOREIGN KEY ("parent_id") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "Capacity" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT NOT NULL,
    "quantity" INTEGER NOT NULL,
    "unit" TEXT NOT NULL,
    "share_depth" INTEGER NOT NULL,
    "expanded" BOOLEAN NOT NULL DEFAULT false,
    "location_type" TEXT NOT NULL,
    "all_day" BOOLEAN NOT NULL DEFAULT false,
    "recurrence" TEXT,
    "custom_recurrence_repeat_every" INTEGER,
    "custom_recurrence_repeat_unit" TEXT,
    "custom_recurrence_end_type" TEXT,
    "custom_recurrence_end_value" TEXT,
    "start_date" TEXT,
    "start_time" TEXT,
    "end_date" TEXT,
    "end_time" TEXT,
    "time_zone" TEXT NOT NULL,
    "max_natural_div" INTEGER NOT NULL,
    "max_percentage_div" REAL NOT NULL,
    "hidden_until_request_accepted" BOOLEAN NOT NULL DEFAULT false,
    "owner_id" TEXT NOT NULL,
    CONSTRAINT "Capacity_owner_id_fkey" FOREIGN KEY ("owner_id") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "CapacityShare" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "share_percentage" REAL NOT NULL,
    "computed_quantity" INTEGER NOT NULL,
    "capacity_id" TEXT NOT NULL,
    "recipient_id" TEXT NOT NULL,
    CONSTRAINT "CapacityShare_capacity_id_fkey" FOREIGN KEY ("capacity_id") REFERENCES "Capacity" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "CapacityShare_recipient_id_fkey" FOREIGN KEY ("recipient_id") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "ShareCache" (
    "provider_id" TEXT NOT NULL,
    "recipient_id" TEXT NOT NULL,
    "share_type" TEXT NOT NULL,
    "depth" INTEGER NOT NULL DEFAULT 0,
    "share_value" REAL NOT NULL,
    "calculated_at" DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY ("provider_id", "recipient_id", "share_type", "depth"),
    CONSTRAINT "ShareCache_provider_id_fkey" FOREIGN KEY ("provider_id") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "ShareCache_recipient_id_fkey" FOREIGN KEY ("recipient_id") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "User" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "name" TEXT,
    "email" TEXT,
    "email_verified" DATETIME,
    "image" TEXT,
    "password" TEXT,
    "role" TEXT DEFAULT 'user'
);

-- CreateTable
CREATE TABLE "Account" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "user_id" TEXT NOT NULL,
    "type" TEXT NOT NULL,
    "provider" TEXT NOT NULL,
    "provider_account_id" TEXT NOT NULL,
    "refresh_token" TEXT,
    "access_token" TEXT,
    "expires_at" INTEGER,
    "token_type" TEXT,
    "scope" TEXT,
    "id_token" TEXT,
    "session_state" TEXT,
    CONSTRAINT "Account_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "User" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "Session" (
    "id" TEXT NOT NULL PRIMARY KEY,
    "session_token" TEXT NOT NULL,
    "user_id" TEXT NOT NULL,
    "expires" DATETIME NOT NULL,
    CONSTRAINT "Session_user_id_fkey" FOREIGN KEY ("user_id") REFERENCES "User" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateTable
CREATE TABLE "VerificationToken" (
    "identifier" TEXT NOT NULL,
    "token" TEXT NOT NULL,
    "expires" DATETIME NOT NULL,

    PRIMARY KEY ("identifier", "token")
);

-- CreateTable
CREATE TABLE "_NodeContributors" (
    "A" TEXT NOT NULL,
    "B" TEXT NOT NULL,
    CONSTRAINT "_NodeContributors_A_fkey" FOREIGN KEY ("A") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE,
    CONSTRAINT "_NodeContributors_B_fkey" FOREIGN KEY ("B") REFERENCES "Node" ("id") ON DELETE CASCADE ON UPDATE CASCADE
);

-- CreateIndex
CREATE UNIQUE INDEX "User_name_key" ON "User"("name");

-- CreateIndex
CREATE UNIQUE INDEX "User_email_key" ON "User"("email");

-- CreateIndex
CREATE UNIQUE INDEX "Account_provider_provider_account_id_key" ON "Account"("provider", "provider_account_id");

-- CreateIndex
CREATE UNIQUE INDEX "Session_session_token_key" ON "Session"("session_token");

-- CreateIndex
CREATE UNIQUE INDEX "_NodeContributors_AB_unique" ON "_NodeContributors"("A", "B");

-- CreateIndex
CREATE INDEX "_NodeContributors_B_index" ON "_NodeContributors"("B");
