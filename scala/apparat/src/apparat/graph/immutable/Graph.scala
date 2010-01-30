/*
 * This file is part of Apparat.
 *
 * Apparat is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Apparat is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Apparat. If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2009 Joa Ebert
 * http://www.joa-ebert.com/
 *
 */
package apparat.graph.immutable

import apparat.graph.{Edge, GraphLike}

class Graph[V](val adjacency: Map[V,List[Edge[V]]]) extends GraphLike[V] {
	def this() = this(Map.empty[V, List[Edge[V]]])

	type G = Graph[V]

	private def newGraph(adjacency: Map[V,List[E]]) = new Graph(adjacency)

	def +(vertex: V) = {
		assert(!contains(vertex))
		newGraph(adjacency + (vertex -> Nil))
	}

	def +(edge: E) = {
		assert(contains(edge.startVertex))
		assert(contains(edge.endVertex))
		assert(!contains(edge))
		newGraph(adjacency updated (edge.startVertex, edge :: adjacency(edge.startVertex)))
	}

	def +(edge: (V, V))(implicit f: (V, V) => Edge[V]): G = ((contains(edge._1), contains(edge._2)) match {
		case (true, true) => this
		case (false, true) => this + edge._1
		case (true, false) => this + edge._2
		case (false, false) => this + edge._1 + edge._2
	}) + f(edge._1, edge._2)

	def -(vertex: V) = {
		assert(contains(vertex))
		newGraph(adjacency filterNot (_._1 == vertex) map { e => e._1 -> (e._2 filterNot (_.endVertex == vertex))})
	}

	def -(edge: E) = {
		assert(contains(edge.startVertex))
		assert(contains(edge.endVertex))
		assert(contains(edge))
		newGraph(adjacency updated (edge.startVertex, adjacency(edge.startVertex) filterNot (_ == edge)))
	}

	def -(edge: (V, V)) = if(contains(edge._1) && contains(edge._2)) {
		newGraph(adjacency updated (edge._1, adjacency(edge._1) filterNot (_.endVertex == edge._2)))
	} else { this }

	override def contains(vertex: V) = adjacency contains vertex

	override def contains(edge: E) = (adjacency get edge.startVertex) match {
		case Some(list) => list exists (_ == edge)
		case None => false
	}

	override def incomingOf(vertex: V) = {
		assert(contains(vertex))
		adjacency flatMap (_._2) filter (_.endVertex == vertex)
	}

	override def outgoingOf(vertex: V) = {
		assert(contains(vertex))
		adjacency(vertex)
	}

	override def outdegreeOf(vertex: V) = outgoingOf(vertex).length

	override def indegreeOf(vertex: V) = {
		assert(contains(vertex))
		adjacency flatMap (_._2) count (_.endVertex == vertex)
	}

	override def predecessorsOf(vertex: V) = incomingOf(vertex) map (_.startVertex)

	override def successorsOf(vertex: V) = outgoingOf(vertex) map (_.endVertex)

	override def verticesIterator = adjacency.keysIterator

	override def edgesIterator = adjacency.valuesIterator flatMap (_.iterator)
}