import { PrismaClient } from '@prisma/client';

// Create a global variable to store the Prisma client instance
const globalForPrisma = global as unknown as { prisma: PrismaClient };

/**
 * Create a singleton Prisma client that can be reused across requests
 * This avoids creating a new client for every request
 */
export const prisma = globalForPrisma.prisma || new PrismaClient();

// Save the client in development, but not in production
if (process.env.NODE_ENV !== 'production') globalForPrisma.prisma = prisma;
 