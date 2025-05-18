import { SvelteKitAuth } from '@auth/sveltekit';
import Credentials from '@auth/sveltekit/providers/credentials';
import { PrismaAdapter } from '@auth/prisma-adapter';
import { redirect, type Handle } from '@sveltejs/kit';
import bcrypt from 'bcryptjs';
import { prisma } from '$lib/server/prisma';
import crypto from 'crypto';
import type { Prisma } from '@prisma/client';

// Extended User type with password
type UserWithPassword = {
	id: string;
	name: string | null;
	email: string | null;
	image: string | null;
	password: string | null;
	role: string | null;
};

/**
 * SvelteKit Auth configuration
 * Uses PrismaAdapter to connect to the database and Credentials provider for username/password login
 */
export const { handle, signIn, signOut } = SvelteKitAuth({
	adapter: PrismaAdapter(prisma),
	session: {
		strategy: 'jwt'
	},
	pages: {
		signIn: '/auth/login',
		error: '/auth/error'
	},
	providers: [
		Credentials({
			credentials: {
				username: { label: 'Username', type: 'text' },
				password: { label: 'Password', type: 'password' }
			},
			async authorize(credentials, request) {
				if (!credentials?.username || !credentials?.password) {
					return null;
				}

				// Find user by username
				const user = await prisma.user.findFirst({
					where: {
						name: credentials.username as string
					}
				});

				if (!user) {
					return null;
				}

				// Type cast to access password field
				const userWithPassword = user as unknown as UserWithPassword;

				// Check if password exists
				if (!userWithPassword.password) {
					return null;
				}

				// Verify password
				const passwordValid = await bcrypt.compare(
					credentials.password.toString(),
					userWithPassword.password
				);

				if (!passwordValid) {
					return null;
				}

				// Return user object that will be stored in JWT
				return {
					id: user.id,
					name: user.name,
					email: user.email,
					image: user.image,
					role: user.role || undefined // Ensure role is string or undefined, not null
				};
			}
		})
	],
	callbacks: {
		async jwt({ token, user }) {
			// Add user ID to the token when first signing in
			if (user) {
				token.id = user.id;
				token.role = user.role;
			}
			return token;
		},
		async session({ session, token }) {
			// Add user ID to the session from the token
			if (token && session.user) {
				session.user.id = token.id as string;
				session.user.role = token.role as string;
			}
			return session;
		}
	},
	secret: process.env.AUTH_SECRET || 'REPLACE_WITH_STRONG_SECRET',
	trustHost: true
});

/**
 * Registration endpoint handler
 * Creates a new user in the database with a hashed password
 */
export const register: Handle = async ({ event, resolve }) => {
	if (event.url.pathname === '/auth/register' && event.request.method === 'POST') {
		try {
			const data = await event.request.json();
			const { username, password } = data;

			// Validate input
			if (!username || !password) {
				return new Response(
					JSON.stringify({ success: false, message: 'Username and password are required' }),
					{ status: 400, headers: { 'Content-Type': 'application/json' } }
				);
			}

			// Check if user already exists
			const existingUser = await prisma.user.findFirst({
				where: {
					name: username
				}
			});

			if (existingUser) {
				return new Response(
					JSON.stringify({ success: false, message: 'Username already exists' }),
					{ status: 400, headers: { 'Content-Type': 'application/json' } }
				);
			}

			// Hash password
			const hashedPassword = await bcrypt.hash(password, 10);

			// Generate unique ID
			const userId = crypto.randomUUID();

			// Create user with raw query to bypass type checking
			await prisma.$executeRaw`
				INSERT INTO User (id, name, email, password, role)
				VALUES (${userId}, ${username}, ${`${username}@example.com`}, ${hashedPassword}, 'user')
			`;

			const createdUser = await prisma.user.findUnique({
				where: { id: userId }
			});

			return new Response(
				JSON.stringify({
					success: true,
					message: 'User registered successfully',
					user: {
						id: createdUser?.id,
						name: createdUser?.name,
						email: createdUser?.email
					}
				}),
				{ status: 200, headers: { 'Content-Type': 'application/json' } }
			);
		} catch (error) {
			console.error('Registration error:', error);
			return new Response(
				JSON.stringify({ success: false, message: 'Registration failed', error: String(error) }),
				{ status: 500, headers: { 'Content-Type': 'application/json' } }
			);
		}
	}

	return resolve(event);
};
 